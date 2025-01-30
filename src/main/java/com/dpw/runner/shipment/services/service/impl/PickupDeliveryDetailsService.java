package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.TIKafkaEventResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.KafkaResponse;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class PickupDeliveryDetailsService implements IPickupDeliveryDetailsService {
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    private JsonHelper jsonHelper;

    private AuditLogService auditLogService;
    private IPartiesDao partiesDao;

    private CommonUtils commonUtils;

    private KafkaProducer producer;

    @Value("${tiKafka.queue}")
    private String senderQueue;

    @Autowired
    public PickupDeliveryDetailsService(CommonUtils commonUtils, IPartiesDao partiesDao, IPickupDeliveryDetailsDao pickupDeliveryDetailsDao, JsonHelper jsonHelper, AuditLogService auditLogService, KafkaProducer producer) {
        this.pickupDeliveryDetailsDao = pickupDeliveryDetailsDao;
        this.jsonHelper = jsonHelper;
        this.auditLogService = auditLogService;
        this.partiesDao = partiesDao;
        this.commonUtils = commonUtils;
        this.producer = producer;
    }

    @Transactional
    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            String resp = "Request is empty for Pickup Delivery create with Request Id " + LoggerHelper.getRequestIdFromMDC();
            log.debug("Request is empty for Pickup Delivery create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse(resp);
        }
        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        try {
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);
            afterSave(pickupDeliveryDetails, true, request);
            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(null)
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("Pickup Delivery Details created successfully for Id {} with Request Id {}", pickupDeliveryDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    private void afterSave(PickupDeliveryDetails pickupDeliveryDetails, boolean isCreate, PickupDeliveryDetailsRequest request) throws RunnerException {
        List<Parties> partiesList = request.getPartiesList();
        Long id = pickupDeliveryDetails.getId();

        if (partiesList != null) {
            List<Parties> updatedParties = partiesDao.updateEntityFromOtherEntity(commonUtils.convertToEntityList(partiesList, Parties.class, isCreate), id, Constants.PICKUP_DELIVERY);
            pickupDeliveryDetails.setPartiesList(updatedParties);
        }
        if (pickupDeliveryDetails.getShipmentId() != null) {
            List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.findByShipmentId(pickupDeliveryDetails.getShipmentId());
            pushToKafka(pickupDeliveryDetailsList, isCreate, pickupDeliveryDetails.getShipmentId());
        }
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildFailedResponse("Request Id is null for Pickup Delivery update with Request Id " + LoggerHelper.getRequestIdFromMDC());
        }

        long id = request.getId();
        Optional<PickupDeliveryDetails> oldEntity = pickupDeliveryDetailsDao.findById(id);
        if (!oldEntity.isPresent()) {
            log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        pickupDeliveryDetails.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(pickupDeliveryDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        afterSave(pickupDeliveryDetails, false, request);

        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Pickup Delivery list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery list with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<PickupDeliveryDetails>, Pageable> tuple = fetchData(request, PickupDeliveryDetails.class);
            Page<PickupDeliveryDetails> pickupDeliveryDetailsPage = pickupDeliveryDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Pickup Delivery list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(pickupDeliveryDetailsPage.getContent()),
                    pickupDeliveryDetailsPage.getTotalPages(),
                    pickupDeliveryDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        return null;
    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery delete with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request Id null for Pickup Delivery delete with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if (!pickupDeliveryDetails.isPresent()) {
                log.debug("pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(pickupDeliveryDetails.get());
            pickupDeliveryDetailsDao.delete(pickupDeliveryDetails.get());

            if (pickupDeliveryDetails.get().getShipmentId() != null) {
                List<PickupDeliveryDetails> pickupDeliveryDetailsList = pickupDeliveryDetailsDao.findByShipmentId(pickupDeliveryDetails.get().getShipmentId());
                pushToKafka(pickupDeliveryDetailsList, false, pickupDeliveryDetails.get().getShipmentId());
            }
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request is empty for Pickup Delivery retrieve with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
                return ResponseHelper.buildFailedResponse("Request Id is null for Pickup Delivery retrieve with Request Id " + LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if (!pickupDeliveryDetails.isPresent()) {
                log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Pickup Delivery details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            PickupDeliveryDetailsResponse response = convertEntityToDto(pickupDeliveryDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage() : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }


    private PickupDeliveryDetailsResponse convertEntityToDto(PickupDeliveryDetails pickupDeliveryDetails) {
        return jsonHelper.convertValue(pickupDeliveryDetails, PickupDeliveryDetailsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<PickupDeliveryDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(pickupDeliveryDetail ->
                responseList.add(convertEntityToDto(pickupDeliveryDetail))
        );
        return responseList;
    }

    public PickupDeliveryDetails convertRequestToEntity(PickupDeliveryDetailsRequest request) {
        return jsonHelper.convertValue(request, PickupDeliveryDetails.class);
    }

    @Async
    public void pushToKafka(List<PickupDeliveryDetails> pickupDeliveryDetails, boolean isCreate, Long shipmentId) {
        try {
            List<IRunnerResponse> pickupDeliveryDetailsResponses = null;
            if (!CommonUtils.listIsNullOrEmpty(pickupDeliveryDetails)) {
                pickupDeliveryDetailsResponses = convertEntityListToDtoList(pickupDeliveryDetails);
            }
            TIKafkaEventResponse tiKafkaEventResponse = new TIKafkaEventResponse();
            tiKafkaEventResponse.setShipmentId(shipmentId);
            tiKafkaEventResponse.setPickupDeliveryDetails(pickupDeliveryDetailsResponses);

            KafkaResponse kafkaResponse = producer.getKafkaResponse(tiKafkaEventResponse, isCreate);
            producer.produceToKafka(jsonHelper.convertToJson(kafkaResponse), senderQueue, UUID.randomUUID().toString());
        }
        catch (Exception e)
        {
            log.error("Error pushing awb to kafka: {}", e.getMessage());
        }
    }
}
