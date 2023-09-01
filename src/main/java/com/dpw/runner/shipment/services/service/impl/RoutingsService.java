package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.dto.request.RoutingsRequest;
import com.dpw.runner.shipment.services.dto.response.RoutingsResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IRoutingsService;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class RoutingsService implements IRoutingsService {
    @Autowired
    private IRoutingsDao routingsDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private AuditLogService auditLogService;

    @Override
    public ResponseEntity<?> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Routing create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        Routings routings = convertRequestToRoutingsEntity(request);
        try {
            routings = routingsDao.save(routings);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(routings)
                            .prevData(null)
                            .parent(Routings.class.getSimpleName())
                            .parentId(routings.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Routing Details created successfully for Id {} with Request Id {}", routings.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(routings));
    }

    @Override
    public ResponseEntity<?> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        RoutingsRequest request = (RoutingsRequest) commonRequestModel.getData();
        if(request == null) {
            log.debug("Request is empty for Routing update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if(request.getId() == null) {
            log.debug("Request Id is null for Routing update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<Routings> oldEntity = routingsDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Routings is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        Routings routings = convertRequestToRoutingsEntity(request);
        routings.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            routings = routingsDao.save(routings);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(routings)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Routings.class))
                            .parent(Routings.class.getSimpleName())
                            .parentId(routings.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the routing details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(routings));
    }

    @Override
    public ResponseEntity<?> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if(request == null) {
                log.error("Request is empty for Routing list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<Routings>, Pageable> tuple = fetchData(request, Routings.class);
            Page<Routings> routingsPage = routingsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Routing list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildListSuccessResponse(
                    convertEntityListToDtoList(routingsPage.getContent()),
                    routingsPage.getTotalPages(),
                    routingsPage.getTotalElements());
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
                log.error("Request is empty for Routing async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<Routings>, Pageable> tuple = fetchData(request, Routings.class);
            Page<Routings> notesPage = routingsDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Routing async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
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
                log.debug("Request is empty for Routings delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.debug("Request Id is null for Routings delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();

            Optional<Routings> routings = routingsDao.findById(id);
            if (routings.isEmpty()) {
                log.debug("Routings is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(routings.get());
            routingsDao.delete(routings.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, Routings.class))
                            .parent(Routings.class.getSimpleName())
                            .parentId(routings.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted Routings details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
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
                log.error("Request is empty for Routings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if(request.getId() == null) {
                log.error("Request Id is null for Routings retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<Routings> notes = routingsDao.findById(id);
            if (notes.isEmpty()) {
                log.debug("Routings is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Routings details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            RoutingsResponse response = convertEntityToDto(notes.get());
            return ResponseHelper.buildSuccessResponse(response);
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private RoutingsResponse convertEntityToDto(Routings notes) {
        return jsonHelper.convertValue(notes, RoutingsResponse.class);
    }

    private Routings convertRequestToRoutingsEntity(RoutingsRequest request) {
        return jsonHelper.convertValue(request, Routings.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<Routings> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}