package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ELDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.ElNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.ELDetailsResponse;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IELDetailsService;
import com.dpw.runner.shipment.services.syncing.Entity.ElDetailsRequestV2;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionAspectSupport;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class ELDetailsService implements IELDetailsService {
    private final IELDetailsDao elDetailsDao;
    private final JsonHelper jsonHelper;
    private final IAuditLogService auditLogService;
    private final ObjectMapper objectMapper;
    private final ModelMapper modelMapper;
    private final IShipmentDao shipmentDao;
    private final SyncConfig syncConfig;

    @Autowired
    public ELDetailsService(IELDetailsDao elDetailsDao,
                            JsonHelper jsonHelper,
                            IAuditLogService auditLogService,
                            ObjectMapper objectMapper,
                            ModelMapper modelMapper,
                            IShipmentDao shipmentDao,
                            SyncConfig syncConfig) {
        this.elDetailsDao = elDetailsDao;
        this.jsonHelper = jsonHelper;
        this.auditLogService = auditLogService;
        this.objectMapper = objectMapper;
        this.modelMapper = modelMapper;
        this.shipmentDao = shipmentDao;
        this.syncConfig = syncConfig;
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        ELDetailsRequest request = (ELDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for EL Details create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        ELDetails elDetails = convertRequestToELDetails(request);
        try {
            elDetails = elDetailsDao.save(elDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(elDetails)
                            .prevData(null)
                            .parent(ELDetails.class.getSimpleName())
                            .parentId(elDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("EL Details created successfully for Id {} with Request Id {}", elDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(elDetails));
    }

    @Transactional
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) throws RunnerException {
        String responseMsg;
        ELDetailsRequest request = (ELDetailsRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for EL details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.debug("Request Id is null for EL details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<ELDetails> oldEntity = elDetailsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("EL Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        ELDetails elDetails = convertRequestToELDetails(request);
        elDetails.setId(oldEntity.get().getId());
        if(elDetails.getGuid() != null && !oldEntity.get().getGuid().equals(elDetails.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }

        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            elDetails = elDetailsDao.save(elDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(elDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ELDetails.class))
                            .parent(ELDetails.class.getSimpleName())
                            .parentId(elDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the EL details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(elDetails));
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for EL details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ELDetails>, Pageable> tuple = fetchData(request, ELDetails.class);
            Page<ELDetails> elDetailsPage = elDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("EL details list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(elDetailsPage.getContent()),
                    elDetailsPage.getTotalPages(),
                    elDetailsPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for EL details async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<ELDetails>, Pageable> tuple = fetchData(request, ELDetails.class);
            Page<ELDetails> elDetailsPage = elDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("EL details async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(elDetailsPage.getContent()),
                    elDetailsPage.getTotalPages(),
                    elDetailsPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }

    }

    @Override
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for EL details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for EL details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<ELDetails> elDetails = elDetailsDao.findById(id);
            if (elDetails.isEmpty()) {
                log.debug("El Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(elDetails.get());
            elDetailsDao.delete(elDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, ELDetails.class))
                            .parent(ELDetails.class.getSimpleName())
                            .parentId(elDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted EL detail for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> validateElNumber(CommonRequestModel requestModel) {
        String responseMsg;
        try {
            ElNumbersRequest request = (ElNumbersRequest) (requestModel.getData());
            if (request == null) {
                log.debug("Request is empty for EL details validate number request with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getElNumber() == null) {
                log.debug("EL number is empty for EL details validate number request with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            String elNumber = request.getElNumber();
            Optional<ELDetails> elDetails = elDetailsDao.findByElNumber(elNumber);
            if (elDetails.isEmpty()) {
                log.debug("El Detail is null for El Number {} with Request Id {}", request.getElNumber(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("EL details exist for EL number {} with Request Id {}", elNumber, LoggerHelper.getRequestIdFromMDC());
            ELDetailsResponse response = convertEntityToDto(elDetails.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> V1ELDetailsCreateAndUpdate(CommonRequestModel commonRequestModel, boolean checkForSync) throws RunnerException {
        ElDetailsRequestV2 elDetailsRequestV2 = (ElDetailsRequestV2) commonRequestModel.getData();
        try {
            if (checkForSync && !Objects.isNull(syncConfig.IS_REVERSE_SYNC_ACTIVE) && !syncConfig.IS_REVERSE_SYNC_ACTIVE) {
                return new ResponseEntity<>(HttpStatus.OK);
            }
            Optional<ELDetails> existingELDetails = elDetailsDao.findByGuid(elDetailsRequestV2.getGuid());
            ELDetails elDetails = modelMapper.map(elDetailsRequestV2, ELDetails.class);
            if (existingELDetails.isPresent()) {
                elDetails.setId(existingELDetails.get().getId());
                elDetails.setShipmentId(existingELDetails.get().getShipmentId());
            } else {
                if (elDetailsRequestV2.getShipmentGuid() != null) {
                    Optional<ShipmentDetails> shipmentDetails = shipmentDao.findByGuid(elDetailsRequestV2.getShipmentGuid());
                    if (shipmentDetails.isPresent())
                        elDetails.setShipmentId(shipmentDetails.get().getId());
                }
            }
            elDetails = elDetailsDao.save(elDetails);
            ELDetailsResponse response = objectMapper.convertValue(elDetails, ELDetailsResponse.class);
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception ex) {
            String responseMsg = ex.getMessage() != null ? ex.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, ex);
            TransactionAspectSupport.currentTransactionStatus().setRollbackOnly();
            throw new RuntimeException(ex);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for EL details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for EL details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<ELDetails> elDetail = elDetailsDao.findById(id);
            if (elDetail.isEmpty()) {
                log.debug("El Detail is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("EL details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            ELDetailsResponse response = convertEntityToDto(elDetail.get());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else{
              return   ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
            }
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private ELDetailsResponse convertEntityToDto(ELDetails elDetails) {
        return jsonHelper.convertValue(elDetails, ELDetailsResponse.class);
    }

    private ELDetails convertRequestToELDetails(ELDetailsRequest request) {
        return jsonHelper.convertValue(request, ELDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<ELDetails> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}
