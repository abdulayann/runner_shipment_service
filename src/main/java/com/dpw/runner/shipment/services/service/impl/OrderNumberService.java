package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IOrderNumberDao;
import com.dpw.runner.shipment.services.dto.request.OrderNumberRequest;
import com.dpw.runner.shipment.services.dto.response.OrderNumberResponse;
import com.dpw.runner.shipment.services.entity.OrderNumber;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IOrderNumberService;
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
public class OrderNumberService implements IOrderNumberService {
    @Autowired
    private IOrderNumberDao orderNumberDao;

    @Autowired
    private JsonHelper jsonHelper;


    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public ResponseEntity<IRunnerResponse> create(CommonRequestModel commonRequestModel) {
        String responseMsg;
        OrderNumberRequest request = (OrderNumberRequest) commonRequestModel.getData();
        if (request == null) {
            log.debug("Request is empty for OrderNumberRequest create with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        OrderNumber OrderNumber = convertRequestToOrderNumberEntity(request);
        try {
            OrderNumber = orderNumberDao.save(OrderNumber);

            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(OrderNumber)
                            .prevData(null)
                            .parent(OrderNumber.class.getSimpleName())
                            .parentId(OrderNumber.getId())
                            .operation(DBOperationType.CREATE.name()).build()
            );

            log.info("Order Number Details created successfully for Id {} with Request Id {}", OrderNumber.getId(), LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(OrderNumber));
    }

    @Override
    public ResponseEntity<IRunnerResponse> update(CommonRequestModel commonRequestModel) {
        String responseMsg;
        OrderNumberRequest request = (OrderNumberRequest) commonRequestModel.getData();
        if (request == null) {
            log.error("Request is empty for Order Number Details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }

        if (request.getId() == null) {
            log.error("Request Id is null for Order Number Details update with Request Id {}", LoggerHelper.getRequestIdFromMDC());
        }
        long id = request.getId();
        Optional<OrderNumber> oldEntity = orderNumberDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Order Number Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        OrderNumber OrderNumber = convertRequestToOrderNumberEntity(request);
        OrderNumber.setId(oldEntity.get().getId());
        try {
            String oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
            OrderNumber = orderNumberDao.save(OrderNumber);

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(OrderNumber)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, OrderNumber.class))
                            .parent(OrderNumber.class.getSimpleName())
                            .parentId(OrderNumber.getId())
                            .operation(DBOperationType.UPDATE.name()).build()
            );
            log.info("Updated the Order Number Details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_UPDATE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
        return ResponseHelper.buildSuccessResponse(convertEntityToDto(OrderNumber));
    }

    @Override
    public ResponseEntity<IRunnerResponse> list(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Order Number Details list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<OrderNumber>, Pageable> tuple = fetchData(request, OrderNumber.class);
            Page<OrderNumber> notesPage = orderNumberDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Order Number Details list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
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
    public CompletableFuture<ResponseEntity<IRunnerResponse>> listAsync(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            ListCommonRequest request = (ListCommonRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Order Number Details async list with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            Pair<Specification<OrderNumber>, Pageable> tuple = fetchData(request, OrderNumber.class);
            Page<OrderNumber> notesPage = orderNumberDao.findAll(tuple.getLeft(), tuple.getRight());
            log.info("Order Number Details async list retrieved successfully for Request Id {} ", LoggerHelper.getRequestIdFromMDC());
            return CompletableFuture.completedFuture(ResponseHelper.buildListSuccessResponse(
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
    public ResponseEntity<IRunnerResponse> delete(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.debug("Request is empty for Order Number Details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.debug("Request Id is null for Order Number Details delete with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<OrderNumber> OrderNumber = orderNumberDao.findById(id);
            if (OrderNumber.isEmpty()) {
                log.debug("OrderNumber is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }

            String oldEntityJsonString = jsonHelper.convertToJson(OrderNumber.get());
            orderNumberDao.delete(OrderNumber.get());

            // audit logs
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .newData(null)
                            .prevData(jsonHelper.readFromJson(oldEntityJsonString, OrderNumber.class))
                            .parent(OrderNumber.class.getSimpleName())
                            .parentId(OrderNumber.get().getId())
                            .operation(DBOperationType.DELETE.name()).build()
            );
            log.info("Deleted Order Number Details for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    @Override
    public ResponseEntity<IRunnerResponse> retrieveById(CommonRequestModel commonRequestModel) {
        String responseMsg;
        try {
            CommonGetRequest request = (CommonGetRequest) commonRequestModel.getData();
            if (request == null) {
                log.error("Request is empty for Order Number Details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            if (request.getId() == null) {
                log.error("Request Id is null for Order Number Details retrieve with Request Id {}", LoggerHelper.getRequestIdFromMDC());
            }
            long id = request.getId();
            Optional<OrderNumber> notes = orderNumberDao.findById(id);
            if (notes.isEmpty()) {
                log.debug("Order Number Details is null for Id {} with Request Id {}", request.getId(), LoggerHelper.getRequestIdFromMDC());
                throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
            }
            log.info("Order Number Details fetched successfully for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            OrderNumberResponse response = convertEntityToDto(notes.get());
            if (request.getIncludeColumns() == null || request.getIncludeColumns().size() == 0)
                return ResponseHelper.buildSuccessResponse(response);
            else
                return ResponseHelper.buildSuccessResponse(PartialFetchUtils.fetchPartialListData(response, request.getIncludeColumns()));
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_RETRIEVE_EXCEPTION_MSG;
            log.error(responseMsg, e);
            return ResponseHelper.buildFailedResponse(responseMsg);
        }
    }

    private OrderNumberResponse convertEntityToDto(OrderNumber notes) {
        return jsonHelper.convertValue(notes, OrderNumberResponse.class);
    }

    private OrderNumber convertRequestToOrderNumberEntity(OrderNumberRequest request) {
        return jsonHelper.convertValue(request, OrderNumber.class);
    }

    private List<IRunnerResponse> convertEntityListToDtoList(final List<OrderNumber> lst) {
        return lst.stream()
                .map(item -> convertEntityToDto(item))
                .collect(Collectors.toList());
    }
}