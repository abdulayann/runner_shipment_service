package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.ReferenceNumbersConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.dto.request.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.response.BulkContainerResponse;
import com.dpw.runner.shipment.services.dto.response.BulkReferenceNumbersResponse;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.dto.response.ReferenceNumbersResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IReferenceNumbersV3Service;
import com.dpw.runner.shipment.services.utils.v3.ReferenceNumbersValidationUtil;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.commons.constants.Constants.NETWORK_TRANSFER;
import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class ReferenceNumbersV3Service implements IReferenceNumbersV3Service {
    @Autowired
    private IReferenceNumbersDao referenceNumbersDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private ReferenceNumbersValidationUtil referenceNumbersValidationUtil;

    @Override
    @Transactional
    public ReferenceNumbersResponse create(ReferenceNumbersRequest request) {

        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting party creation | Request ID: {} | Request Body: {}", requestId, request);

        //Convert DTO to entity
        ReferenceNumbers referenceNumber = jsonHelper.convertValue(request, ReferenceNumbers.class);
        log.debug("Converted reference numbers request to entity | Entity:  {}", referenceNumber);

        //Save to DB
        ReferenceNumbers savedReferenceNumber = referenceNumbersDao.save(referenceNumber);
        log.info("Saved reference number entity to DB | Reference Number ID: {} | Request ID: {}", savedReferenceNumber.getId(), requestId);

        //Audit Logging
        recordAuditLogs(null, List.of(referenceNumber), DBOperationType.CREATE);

        ReferenceNumbersResponse response = jsonHelper.convertValue(savedReferenceNumber, ReferenceNumbersResponse.class);
        log.info("Returning reference number response | Reference Number ID: {} | Response: {}", savedReferenceNumber.getId(), response);

        return response;
    }

    @Override
    @Transactional
    public ReferenceNumbersResponse update(ReferenceNumbersRequest request) {

        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting reference number updation | Request ID: {} | Request Body: {}", requestId, request);

        // Validate the incoming request to ensure all mandatory fields are present
        referenceNumbersValidationUtil.validateUpdateRequest(request);

        Long id = request.getId();

        //Fetching the saved entity from db
        Optional<ReferenceNumbers> oldEntity = referenceNumbersDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(ReferenceNumbersConstants.REFERENCE_NUMBER_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        //Convert DTO to entity
        ReferenceNumbers referenceNumber = jsonHelper.convertValue(request, ReferenceNumbers.class);
        log.debug("Converted reference number request to entity | Entity:  {}", referenceNumber);

        referenceNumber.setId(oldEntity.get().getId());

        //Updating existing party
        ReferenceNumbers updatedReferenceNumber = referenceNumbersDao.save(referenceNumber);
        log.info("Saved updated reference number entity to DB | Party ID: {} | Request ID: {}", updatedReferenceNumber.getId(), requestId);

        //Audit Logging
        recordAuditLogs(List.of(oldEntity.get()), List.of(updatedReferenceNumber), DBOperationType.UPDATE);

        ReferenceNumbersResponse response = jsonHelper.convertValue(updatedReferenceNumber, ReferenceNumbersResponse.class);
        log.info("Returning updated reference number response | Reference Number ID: {} | Response: {}", updatedReferenceNumber.getId(), response);

        return response;
    }

    @Override
    @Transactional
    public BulkReferenceNumbersResponse updateBulk(List<ReferenceNumbersRequest> requests) {

        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting reference number updation | Request ID: {} | Request Body: {}", requestId, requests);

        // Validate the incoming request to ensure all mandatory fields are present
        referenceNumbersValidationUtil.validateUpdateBulkRequest(requests);

        // Convert the request DTOs to entity models for persistence
        List<ReferenceNumbers> originalReferenceNumbers = jsonHelper.convertValueToList(requests, ReferenceNumbers.class);

        // Save the updated ref nums to the database
        List<ReferenceNumbers> updatedReferenceNumbers = referenceNumbersDao.saveAll(originalReferenceNumbers);

        //runAsyncPostSaveOperations(updatedReferenceNumbers, module);

        // Add audit logs for all updated containers
        recordAuditLogs(originalReferenceNumbers, updatedReferenceNumbers, DBOperationType.UPDATE);

        // Convert saved entities into response DTOs
        List<ReferenceNumbersResponse> referenceNumbersResponses = jsonHelper.convertValueToList(updatedReferenceNumbers, ReferenceNumbersResponse.class);

        // Build and return the response
        return BulkReferenceNumbersResponse.builder()
                .referenceNumbersResponseList(referenceNumbersResponses)
                .message(prepareBulkUpdateMessage(referenceNumbersResponses))
                .build();
    }

    @Override
    public List<ReferenceNumbersResponse> list(ListCommonRequest request, String xSource) {
        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting reference number listing | Request ID: {} | Request Body: {}", requestId, request);

        Pair<Specification<ReferenceNumbers>, Pageable> tuple = fetchData(request, ReferenceNumbers.class);

        Page<ReferenceNumbers> referenceNumbersPage;

        //Fetch all party list from db
        if(Objects.equals(xSource, NETWORK_TRANSFER))
            referenceNumbersPage = referenceNumbersDao.findAllWithoutTenantFilter(tuple.getLeft(), tuple.getRight());
        else
            referenceNumbersPage = referenceNumbersDao.findAll(tuple.getLeft(), tuple.getRight());

        log.info("Reference numbers list retrieved successfully for Request Id: {} | List content: {}", LoggerHelper.getRequestIdFromMDC(), referenceNumbersPage.getContent());
        return convertEntityListToDtoList(referenceNumbersPage.getContent());
    }


    @Override
    @Transactional
    public ReferenceNumbersResponse delete(ReferenceNumbersRequest request) {

        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting reference number deletion | Request ID: {} | Request Body: {}", requestId, request);

        // Validate the incoming request to ensure all mandatory fields are present
        referenceNumbersValidationUtil.validateUpdateRequest(request);

        Long id = request.getId();

        //Fetching the saved entity from db
        Optional<ReferenceNumbers> referenceNumber = referenceNumbersDao.findById(id);
        if (referenceNumber.isEmpty()) {
            log.debug(ReferenceNumbersConstants.REFERENCE_NUMBER_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        //Delete party from db
        referenceNumbersDao.delete(referenceNumber.get());

        //Audit Logging
        recordAuditLogs(List.of(referenceNumber.get()), null, DBOperationType.DELETE);

        log.info("Deleted reference number for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        return ReferenceNumbersResponse.builder().message("Reference number deleted successfully!").build();
    }

    private ReferenceNumbersResponse convertEntityToDto(ReferenceNumbers referenceNumbers) {
        return jsonHelper.convertValue(referenceNumbers, ReferenceNumbersResponse.class);
    }


    private List<ReferenceNumbersResponse> convertEntityListToDtoList(final List<ReferenceNumbers> lst) {
        return lst.stream()
                .map(this::convertEntityToDto)
                .toList();
    }

    private void recordAuditLogs(List<ReferenceNumbers> oldReferenceNumbers, List<ReferenceNumbers> newReferenceNumbers, DBOperationType operationType) {
        Map<Long, ReferenceNumbers> oldReferenceNumbersMap = Optional.ofNullable(oldReferenceNumbers).orElse(List.of()).stream()
                .filter(referenceNumber -> referenceNumber.getId() != null)
                .collect(Collectors.toMap(ReferenceNumbers::getId, Function.identity()));

        Map<Long, ReferenceNumbers> newReferenceNumbersMap = Optional.ofNullable(newReferenceNumbers).orElse(List.of()).stream()
                .filter(referenceNumber -> referenceNumber.getId() != null)
                .collect(Collectors.toMap(ReferenceNumbers::getId, Function.identity()));

        // Decide the relevant set of IDs based on operation
        Set<Long> idsToProcess = switch (operationType) {
            case CREATE -> newReferenceNumbersMap.keySet();
            case DELETE -> oldReferenceNumbersMap.keySet();
            case UPDATE -> {
                Set<Long> ids = new HashSet<>(oldReferenceNumbersMap.keySet());
                ids.retainAll(newReferenceNumbersMap.keySet()); // only intersecting IDs
                yield ids;
            }
            default -> throw new IllegalStateException("Unexpected value: " + operationType);
        };

        for (Long id : idsToProcess) {
            try {
                ReferenceNumbers oldData = oldReferenceNumbersMap.get(id);
                ReferenceNumbers newData = newReferenceNumbersMap.get(id);

                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId())
                                .userName(UserContext.getUser().getUsername())
                                .prevData(oldData)
                                .newData(newData)
                                .parent(ReferenceNumbers.class.getSimpleName())
                                .parentId(id)
                                .operation(operationType.name())
                                .build()
                );
            } catch (Exception ex) {
                log.error("Failed to add audit log for reference number ID {} and operation [{}]: {}", id, operationType, ex.getMessage(), ex);
            }
        }
    }

    private String prepareBulkUpdateMessage(List<ReferenceNumbersResponse> referenceNumbersResponses) {
        String message;

        // If more than one ref nums was updated, return a generic bulk success message
        if (referenceNumbersResponses.size() > 1) {
            message = "Bulk edit success! All selected reference numbers have been updated.";
        } else {
            // For a single ref num update
            ReferenceNumbersResponse response = referenceNumbersResponses.get(0);
            String referenceNumber = response.getReferenceNumber();

            message = String.format("Reference number %s updated successfully!", referenceNumber);
        }

        return message;
    }
}
