package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IPickupDeliveryDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.PickupDeliveryDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.PickupDeliveryDetailsResponse;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IPickupDeliveryDetailsService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.syncing.Entity.PickupDeliveryDetailsRequestV2;
import com.dpw.runner.shipment.services.syncing.constants.SyncingConstants;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSync;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@SuppressWarnings("ALL")
@Service
@Slf4j
public class PickupDeliveryDetailsService implements IPickupDeliveryDetailsService {
    @Autowired
    private IPickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IShipmentSync shipmentSync;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentService shipmentService;
    @Lazy
    @Autowired
    private ISyncQueueService syncQueueService;
    @Autowired
    private SyncConfig syncConfig;

    @Transactional
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PickupDeliveryDetailsRequest request = null;
        request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Pickup Delivery create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        try {
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(pickupDeliveryDetails)
                            .prevData(null)
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );
            log.info("Pickup Delivery Details created successfully for Id {} with Request Id {}", pickupDeliveryDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    @Transactional
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        PickupDeliveryDetailsRequest request = (PickupDeliveryDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Pickup Delivery update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<PickupDeliveryDetails> oldEntity = pickupDeliveryDetailsDao.findById(id);
        if(!oldEntity.isPresent()) {
            log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        PickupDeliveryDetails pickupDeliveryDetails = convertRequestToEntity(request);
        pickupDeliveryDetails.setId(oldEntity.get().getId());
        if(pickupDeliveryDetails.getGuid() != null && !oldEntity.get().getGuid().equals(pickupDeliveryDetails.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            pickupDeliveryDetails = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(pickupDeliveryDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );

            log.info("Updated the pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(pickupDeliveryDetails));
    }

    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Pickup Delivery list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<PickupDeliveryDetails>, Pageable> tuple = fetchData(request, PickupDeliveryDetails.class);
            Page<PickupDeliveryDetails> pickupDeliveryDetailsPage  = pickupDeliveryDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Pickup Delivery list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(pickupDeliveryDetailsPage.getContent()),
                    pickupDeliveryDetailsPage.getTotalPages(),
                    pickupDeliveryDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Pickup Delivery async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            // construct specifications for filter request
            Pair<Specification<PickupDeliveryDetails>, Pageable> tuple = fetchData(request, PickupDeliveryDetails.class);
            Page<PickupDeliveryDetails> pickupDeliveryDetailsPage  = pickupDeliveryDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Pickup Delivery async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(pickupDeliveryDetailsPage.getContent()),
                    pickupDeliveryDetailsPage.getTotalPages(),
                    pickupDeliveryDetailsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Pickup Delivery delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if(!pickupDeliveryDetails.isPresent()) {
                log.debug("pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(pickupDeliveryDetails.get());
            pickupDeliveryDetailsDao.delete(pickupDeliveryDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, PickupDeliveryDetails.class))
                            .parent(PickupDeliveryDetails.class.getSimpleName())
                            .parentId(pickupDeliveryDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted pickup delivery details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Pickup Delivery retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(id);
            if(!pickupDeliveryDetails.isPresent()) {
                log.debug("Pickup Delivery Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Pickup Delivery details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            PickupDeliveryDetailsResponse response = convertEntityToDto(pickupDeliveryDetails.get());
            if(request.getIncludeColumns()==null||request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> V1PickupDeliveryCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws Exception {
        PickupDeliveryDetailsRequestV2 pickupDeliveryDetailsRequestV2 = (PickupDeliveryDetailsRequestV2) commonRequestModel.getData();
        if(pickupDeliveryDetailsRequestV2 == null || pickupDeliveryDetailsRequestV2.getShipmentGuid() == null) {
            throw new Exception("Request guid is null");
        }
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return syncQueueService.saveSyncRequest(SyncingConstants.PICKUP_DELIVERY, StringUtility.convertToString(pickupDeliveryDetailsRequestV2.getShipmentGuid()), pickupDeliveryDetailsRequestV2);
            }
            Optional<ShipmentDetails> existingShipment = shipmentDao.findByGuid(pickupDeliveryDetailsRequestV2.getShipmentGuid());
            if(existingShipment == null || existingShipment.get() == null) {
                log.debug("Shipment Details is null for Guid {} with Request Id {}", pickupDeliveryDetailsRequestV2.getShipmentGuid(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            PickupDeliveryDetails pickupDeliveryDetails = modelMapper.map(pickupDeliveryDetailsRequestV2, PickupDeliveryDetails.class);
            ShipmentDetails oldShipment = existingShipment.get();

            if(pickupDeliveryDetailsRequestV2.getType().equals(Constants.PICK_UP))
                oldShipment.setPickupDetails(pickupDeliveryDetails);
            else if(pickupDeliveryDetailsRequestV2.getType().equals(Constants.DELIVERY))
                oldShipment.setDeliveryDetails(pickupDeliveryDetails);
            else {
                log.debug("Type provided is not correct");
                throw new Exception();
            }

            oldShipment = shipmentDao.save(oldShipment, true);
            shipmentService.pushShipmentDataToDependentService(oldShipment, false);
            try {
                shipmentSync.sync(oldShipment, null, null, UUID.randomUUID().toString(), false);
            } catch (Exception e) {
                log.error("Error performing sync on shipment entity, {}", e);
            }
            PickupDeliveryDetailsResponse pickupDeliveryDetailsResponse = new PickupDeliveryDetailsResponse();
            if(pickupDeliveryDetailsRequestV2.getType().equals(Constants.PICK_UP))
                pickupDeliveryDetailsResponse = jsonHelper.convertValue(oldShipment.getPickupDetails(), PickupDeliveryDetailsResponse.class);
            else if(pickupDeliveryDetailsRequestV2.getType().equals(Constants.DELIVERY))
                pickupDeliveryDetailsResponse = jsonHelper.convertValue(oldShipment.getDeliveryDetails(), PickupDeliveryDetailsResponse.class);

            return ResponseHelper.buildSuccessResponse(pickupDeliveryDetailsResponse);
        } catch (Exception e) {
            String responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(e);
        }
    }

    private PickupDeliveryDetailsResponse convertEntityToDto(PickupDeliveryDetails pickupDeliveryDetails) {
        return jsonHelper.convertValue(pickupDeliveryDetails, PickupDeliveryDetailsResponse.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(List<PickupDeliveryDetails> lst) {
        List<IRunnerResponse> responseList = new ArrayList<>();
        lst.forEach(pickupDeliveryDetail -> responseList.add(convertEntityToDto(pickupDeliveryDetail)));
        return responseList;
    }

    public PickupDeliveryDetails convertRequestToEntity(PickupDeliveryDetailsRequest request) {
        return jsonHelper.convertValue(request, PickupDeliveryDetails.class);
    }
}
