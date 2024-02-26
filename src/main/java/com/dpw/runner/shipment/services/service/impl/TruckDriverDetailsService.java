package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.request.TruckDriverDetailsRequest;
import com.dpw.runner.shipment.services.dto.response.TruckDriverDetailsResponse;
import com.dpw.runner.shipment.services.entity.TruckDriverDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.ITruckDriverDetailsService;
import com.dpw.runner.shipment.services.utils.PartialFetchUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class TruckDriverDetailsService implements ITruckDriverDetailsService {
    @Autowired
    private ITruckDriverDetailsDao truckDriverDetailsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;
    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        TruckDriverDetailsRequest request = (TruckDriverDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Truck Driver Details create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        TruckDriverDetails truckDriverDetails = convertRequestToTruckDriverDetailsEntity(request);
        try {
            truckDriverDetails = truckDriverDetailsDao.save(truckDriverDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(truckDriverDetails)
                            .prevData(null)
                            .parent(TruckDriverDetails.class.getSimpleName())
                            .parentId(truckDriverDetails.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Truck Driver Details created successfully for Id {} with Request Id {}", truckDriverDetails.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(truckDriverDetails));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        TruckDriverDetailsRequest request = (TruckDriverDetailsRequest) commonRequestModel.getData();
        if(request == null) {
            log.error("Request is empty for Truck Driver Details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.error("Request Id is null for Truck Driver Details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<TruckDriverDetails> oldEntity = truckDriverDetailsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Truck Driver Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        TruckDriverDetails truckDriverDetails = convertRequestToTruckDriverDetailsEntity(request);
        truckDriverDetails.setId(oldEntity.get().getId());

        if(truckDriverDetails.getGuid() != null && !oldEntity.get().getGuid().equals(truckDriverDetails.getGuid())) {
            throw new RunnerException("Provided GUID doesn't match with the existing one !");
        }
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            truckDriverDetails = truckDriverDetailsDao.save(truckDriverDetails);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(truckDriverDetails)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, TruckDriverDetails.class))
                            .parent(TruckDriverDetails.class.getSimpleName())
                            .parentId(truckDriverDetails.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the Truck Driver Details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(truckDriverDetails));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Truck Driver Details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<TruckDriverDetails>, Pageable> tuple = fetchData(request, TruckDriverDetails.class);
            Page<TruckDriverDetails> notesPage = truckDriverDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Truck Driver Details list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(notesPage.getContent()),
                    notesPage.getTotalPages(),
                    notesPage.getTotalElements());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    @Async
    public CompletableFuture<ResponseEntity<?>> listAsync(CommonRequestModel commonRequestModel){
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Truck Driver Details async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<TruckDriverDetails>, Pageable> tuple = fetchData(request, TruckDriverDetails.class);
            Page<TruckDriverDetails> notesPage = truckDriverDetailsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Truck Driver Details async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture( ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(notesPage.getContent()),
                    notesPage.getTotalPages(),
                    notesPage.getTotalElements()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_LIST_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return CompletableFuture.completedFuture(ResponseHelper.buildFailedResponse(responseMsg));
        }
    }

    @Override
    public ResponseEntity<?> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.debug("Request is empty for Truck Driver Details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Truck Driver Details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<TruckDriverDetails> truckDriverDetails = truckDriverDetailsDao.findById(id);
            if (truckDriverDetails.isEmpty()) {
                log.debug("TruckDriverDetails is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(truckDriverDetails.get());
            truckDriverDetailsDao.delete(truckDriverDetails.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, TruckDriverDetails.class))
                            .parent(TruckDriverDetails.class.getSimpleName())
                            .parentId(truckDriverDetails.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted Truck Driver Details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<?> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Truck Driver Details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Truck Driver Details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<TruckDriverDetails> notes = truckDriverDetailsDao.findById(id);
            if (notes.isEmpty()) {
                log.debug("Truck Driver Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Truck Driver Details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            TruckDriverDetailsResponse response = convertEntityToDto(notes.get());
            if(request.getIncludeColumns()==null || request.getIncludeColumns().isEmpty())
                return ResponseHelper.buildSuccessResponse(response);
            else return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private TruckDriverDetailsResponse convertEntityToDto(TruckDriverDetails notes) {
        return jsonHelper.convertValue(notes, TruckDriverDetailsResponse.class);
    }

    private TruckDriverDetails convertRequestToTruckDriverDetailsEntity(TruckDriverDetailsRequest request) {
        return jsonHelper.convertValue(request, TruckDriverDetails.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<TruckDriverDetails> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}