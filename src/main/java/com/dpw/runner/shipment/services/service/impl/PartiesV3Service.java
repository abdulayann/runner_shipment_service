package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPartiesDao;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IPartiesV3Service;
import com.dpw.runner.shipment.services.utils.v3.PartiesValidationUtil;
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

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;

@Service
@Slf4j
public class PartiesV3Service implements IPartiesV3Service {
    @Autowired
    private IPartiesDao partiesDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private PartiesValidationUtil partiesValidationUtil;

    @Override
    @Transactional
    public PartiesResponse create(PartiesRequest request) {

        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting party creation | Request ID: {} | Request Body: {}", requestId, request);

        //Convert DTO to entity
        Parties party = jsonHelper.convertValue(request, Parties.class);
        log.debug("Converted parties request to entity | Entity:  {}", party);

        //Save to DB
        Parties savedParty = partiesDao.save(party);
        log.info("Saved party entity to DB | Party ID: {} | Request ID: {}", savedParty.getId(), requestId);

        //Audit Logging
        recordAuditLogs(null, List.of(party), DBOperationType.CREATE);

        PartiesResponse response = jsonHelper.convertValue(savedParty, PartiesResponse.class);
        log.info("Returning party response | Party ID: {} | Response: {}", savedParty.getId(), response);

        return response;
    }

    @Override
    @Transactional
    public PartiesResponse update(PartiesRequest request) {

        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting party updation | Request ID: {} | Request Body: {}", requestId, request);

        // Validate the incoming request to ensure all mandatory fields are present
        partiesValidationUtil.validateUpdateRequest(request);

        Long id = request.getId();

        //Fetching the saved entity from db
        Optional<Parties> oldEntity = partiesDao.findById(id);
        if (oldEntity.isEmpty()) {
            log.debug(PartiesConstants.PARTY_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        //Convert DTO to entity
        Parties party = jsonHelper.convertValue(request, Parties.class);
        log.debug("Converted parties request to entity | Entity:  {}", party);

        party.setId(oldEntity.get().getId());

        //Updating existing party
        Parties updatedParty = partiesDao.save(party);
        log.info("Saved updated party entity to DB | Party ID: {} | Request ID: {}", updatedParty.getId(), requestId);

        //Audit Logging
        recordAuditLogs(List.of(oldEntity.get()), List.of(updatedParty), DBOperationType.UPDATE);

        PartiesResponse response = jsonHelper.convertValue(updatedParty, PartiesResponse.class);
        log.info("Returning updated party response | Party ID: {} | Response: {}", updatedParty.getId(), response);

        return response;
    }

    @Override
    public List<PartiesResponse> list(ListCommonRequest request) {
        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting party listing | Request ID: {} | Request Body: {}", requestId, request);

        Pair<Specification<Parties>, Pageable> tuple = fetchData(request, Parties.class);

        //Fetch all party list from db
        Page<Parties> partiesPage = partiesDao.findAll(tuple.getLeft(), tuple.getRight());

        log.info("Parties list retrieved successfully for Request Id: {} | List content: {}", LoggerHelper.getRequestIdFromMDC(), partiesPage.getContent());
        return convertEntityListToDtoList(partiesPage.getContent());
    }


    @Override
    public PartiesResponse delete(PartiesRequest request) {

        String requestId = LoggerHelper.getRequestIdFromMDC();
        log.info("Starting party listing | Request ID: {} | Request Body: {}", requestId, request);

        // Validate the incoming request to ensure all mandatory fields are present
        partiesValidationUtil.validateUpdateRequest(request);

        Long id = request.getId();

        //Fetching the saved entity from db
        Optional<Parties> party = partiesDao.findById(id);
        if (party.isEmpty()) {
            log.debug(PartiesConstants.PARTY_RETRIEVE_BY_ID_ERROR, request.getId(), LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }

        //Delete party from db
        partiesDao.delete(party.get());

        //Audit Logging
        recordAuditLogs(List.of(party.get()), null, DBOperationType.DELETE);

        log.info("Deleted party for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
        return PartiesResponse.builder().message("Party deleted successfully!").build();
    }

    private PartiesResponse convertEntityToDto(Parties parties) {
        return jsonHelper.convertValue(parties, PartiesResponse.class);
    }


    private List<PartiesResponse> convertEntityListToDtoList(final List<Parties> lst) {
        return lst.stream()
                .map(this::convertEntityToDto)
                .toList();
    }

    private void recordAuditLogs(List<Parties> oldParties, List<Parties> newParties, DBOperationType operationType) {
        Map<Long, Parties> oldPartiesMap = Optional.ofNullable(oldParties).orElse(List.of()).stream()
                .filter(party -> party.getId() != null)
                .collect(Collectors.toMap(Parties::getId, Function.identity()));

        Map<Long, Parties> newPartiesMap = Optional.ofNullable(newParties).orElse(List.of()).stream()
                .filter(party -> party.getId() != null)
                .collect(Collectors.toMap(Parties::getId, Function.identity()));

        // Decide the relevant set of IDs based on operation
        Set<Long> idsToProcess = switch (operationType) {
            case CREATE -> newPartiesMap.keySet();
            case DELETE -> oldPartiesMap.keySet();
            case UPDATE -> {
                Set<Long> ids = new HashSet<>(oldPartiesMap.keySet());
                ids.retainAll(newPartiesMap.keySet()); // only intersecting IDs
                yield ids;
            }
            default -> throw new IllegalStateException("Unexpected value: " + operationType);
        };

        for (Long id : idsToProcess) {
            try {
                Parties oldData = oldPartiesMap.get(id);
                Parties newData = newPartiesMap.get(id);

                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId())
                                .userName(UserContext.getUser().getUsername())
                                .prevData(oldData)
                                .newData(newData)
                                .parent(Containers.class.getSimpleName())
                                .parentId(id)
                                .operation(operationType.name())
                                .build()
                );
            } catch (Exception ex) {
                log.error("Failed to add audit log for container ID {} and operation [{}]: {}", id, operationType, ex.getMessage(), ex);
            }
        }
    }
}
