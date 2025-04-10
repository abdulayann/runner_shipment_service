package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IReferenceNumbersDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IReferenceNumbersRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.lang.reflect.InvocationTargetException;
import java.sql.Ref;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class ReferenceNumbersDao implements IReferenceNumbersDao {
    public static final String REFERENCE_NUMBER_IS_NULL_FOR_ID_MSG = "Reference number is null for Id {}";
    @Autowired
    private IReferenceNumbersRepository referenceNumbersRepository;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public ReferenceNumbers save(ReferenceNumbers referenceNumbers) {
        return referenceNumbersRepository.save(referenceNumbers);
    }
    @Override
    public List<ReferenceNumbers> saveAll(List<ReferenceNumbers> referenceNumbersList) {
        return referenceNumbersRepository.saveAll(referenceNumbersList);
    }

    @Override
    public Page<ReferenceNumbers> findAll(Specification<ReferenceNumbers> spec, Pageable pageable) {
        return referenceNumbersRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<ReferenceNumbers> findById(Long id) {
        return referenceNumbersRepository.findById(id);
    }

    @Override
    public void delete(ReferenceNumbers referenceNumbers) {
        referenceNumbersRepository.delete(referenceNumbers);
    }

    @Override
    public List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId) throws RunnerException {
        String responseMsg;
        List<ReferenceNumbers> responseReferenceNumbers = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            List<ReferenceNumbers> routings = findByShipmentId(shipmentId);
            Map<Long, ReferenceNumbers> hashMap = routings.stream()
                        .collect(Collectors.toMap(ReferenceNumbers::getId, Function.identity()));
            Map<Long, ReferenceNumbers> copyHashMap = new HashMap<>(hashMap);
            List<ReferenceNumbers> referenceNumbersRequests = new ArrayList<>();
            if (referenceNumbersList != null && !referenceNumbersList.isEmpty()) {
                for (ReferenceNumbers request : referenceNumbersList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    referenceNumbersRequests.add(request);
                }
                responseReferenceNumbers = saveEntityFromShipment(referenceNumbersRequests, shipmentId, copyHashMap);
            }
            deleteReferenceNumbers(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseReferenceNumbers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    public List<ReferenceNumbers> findByShipmentId(Long shipmentId) {
        return referenceNumbersRepository.findByShipmentId(shipmentId);
    }

    @Override
    public List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId) {
        List<ReferenceNumbers> res = new ArrayList<>();
        for(ReferenceNumbers req : referenceNumbersRequests){
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(req.getId() != null){
                long id = req.getId();
                Optional<ReferenceNumbers> oldEntity = findById(id);
                if (oldEntity.isEmpty()) {
                    log.debug(REFERENCE_NUMBER_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ReferenceNumbers.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }

    @Override
    public List<ReferenceNumbers> saveEntityFromBooking(List<ReferenceNumbers> referenceNumbersRequests, Long bookingId) {
        List<ReferenceNumbers> res = new ArrayList<>();
        Map<Long, ReferenceNumbers> hashMap = referenceNumberMap(bookingId);
        for (ReferenceNumbers req : referenceNumbersRequests) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug(REFERENCE_NUMBER_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(hashMap.get(id));
                operation = DBOperationType.UPDATE.name();
            }
            req.setBookingId(bookingId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Packing.class) : null)
                                .parent(CustomerBooking.class.getSimpleName())
                                .parentId(bookingId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }


    @Override
    public List<ReferenceNumbers> updateEntityFromBooking(List<ReferenceNumbers> referenceNumbersList, Long bookingId) throws RunnerException {
        String responseMsg;
        List<ReferenceNumbers> responsePackings = new ArrayList<>();
        try {
            Map<Long, ReferenceNumbers> hashMap = referenceNumberMap(bookingId);
            List<ReferenceNumbers> referernceNumbersRequestList = new ArrayList<>();
            if (referenceNumbersList != null && !referenceNumbersList.isEmpty()) {
                for (ReferenceNumbers request : referenceNumbersList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    referernceNumbersRequestList.add(request);
                }
                responsePackings = saveEntityFromBooking(referernceNumbersRequestList, bookingId);
            }
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private Map<Long, ReferenceNumbers> referenceNumberMap(Long bookingId) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
        Page<ReferenceNumbers> referenceNumbersPage = findAll(pair.getLeft(), pair.getRight());
        return referenceNumbersPage.stream()
                .collect(Collectors.toMap(ReferenceNumbers::getId, Function.identity()));
    }


    @Override
    public List<ReferenceNumbers> saveEntityFromShipment(List<ReferenceNumbers> referenceNumbersRequests, Long shipmentId, Map<Long, ReferenceNumbers> hashMap) {
        List<ReferenceNumbers> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for(ReferenceNumbers req : referenceNumbersRequests){
            if(req.getId() != null){
                long id = req.getId();
                if (!hashMap.containsKey(id)) {
                    log.debug(REFERENCE_NUMBER_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(hashMap.get(id).getCreatedAt());
                req.setCreatedBy(hashMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(hashMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setShipmentId(shipmentId);
            res.add(req);
        }
        res = saveAll(res);
        for (var req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (oldEntityJsonStringMap.containsKey(req.getId())) {
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ReferenceNumbers.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    @Override
    public List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId) throws RunnerException {
        String responseMsg;
        List<ReferenceNumbers> responseReferenceNumbers = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            Map<Long, ReferenceNumbers> hashMap;
            ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
            Pair<Specification<ReferenceNumbers>, Pageable> pair = fetchData(listCommonRequest, ReferenceNumbers.class);
            Page<ReferenceNumbers> routings = findAll(pair.getLeft(), pair.getRight());
            hashMap = routings.stream()
                    .collect(Collectors.toMap(ReferenceNumbers::getId, Function.identity()));
            Map<Long, ReferenceNumbers> copyHashMap = new HashMap<>(hashMap);
            List<ReferenceNumbers> referenceNumbersRequests = new ArrayList<>();
            if (!referenceNumbersList.isEmpty()) {
                for (ReferenceNumbers request : referenceNumbersList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    referenceNumbersRequests.add(request);
                }
                responseReferenceNumbers = saveEntityFromConsole(referenceNumbersRequests, consolidationId, copyHashMap);
            }
            deleteReferenceNumbers(hashMap, ConsolidationDetails.class.getSimpleName(), consolidationId);
            return responseReferenceNumbers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<ReferenceNumbers> updateEntityFromConsole(List<ReferenceNumbers> referenceNumbersList, Long consolidationId, List<ReferenceNumbers> oldEntityList) throws RunnerException {
        String responseMsg;
        Map<UUID, ReferenceNumbers> referenceNumbersMap = new HashMap<>();
        if(oldEntityList != null && !oldEntityList.isEmpty()) {
            for (ReferenceNumbers entity:
                    oldEntityList) {
                referenceNumbersMap.put(entity.getGuid(), entity);
            }
        }

        List<ReferenceNumbers> responseReferenceNumbers = new ArrayList<>();
        try {
            ReferenceNumbers oldEntity;
            List<ReferenceNumbers> referenceNumbersRequests = new ArrayList<>();
            if (referenceNumbersList != null && !referenceNumbersList.isEmpty()) {
                for (ReferenceNumbers request : referenceNumbersList) {
                    oldEntity = referenceNumbersMap.get(request.getGuid());
                    if(oldEntity != null) {
                        referenceNumbersMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    referenceNumbersRequests.add(request);
                }
                responseReferenceNumbers = saveEntityFromConsole(referenceNumbersRequests, consolidationId);
            }
            Map<Long, ReferenceNumbers> hashMap = new HashMap<>();
            referenceNumbersMap.forEach((s, referenceNumbers) ->  hashMap.put(referenceNumbers.getId(), referenceNumbers));
            deleteReferenceNumbers(hashMap, ConsolidationDetails.class.getSimpleName(), consolidationId);
            return responseReferenceNumbers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<ReferenceNumbers> saveEntityFromConsole(List<ReferenceNumbers> referenceNumbersRequests, Long consolidationId) {
        List<ReferenceNumbers> res = new ArrayList<>();
        for(ReferenceNumbers req : referenceNumbersRequests){
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(req.getId() != null){
                long id = req.getId();
                Optional<ReferenceNumbers> oldEntity = findById(id);
                if (oldEntity.isEmpty()) {
                    log.debug(REFERENCE_NUMBER_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ReferenceNumbers.class) : null)
                                .parent(ConsolidationDetails.class.getSimpleName())
                                .parentId(consolidationId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }
    @Override
    public List<ReferenceNumbers> saveEntityFromConsole(List<ReferenceNumbers> referenceNumbersRequests, Long consolidationId, Map<Long, ReferenceNumbers> hashMap) {
        List<ReferenceNumbers> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for(ReferenceNumbers req : referenceNumbersRequests){
            if(req.getId() != null){
                long id = req.getId();
                if (!hashMap.containsKey(id)) {
                    log.debug(REFERENCE_NUMBER_IS_NULL_FOR_ID_MSG, req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                String oldEntityJsonString = jsonHelper.convertToJson(hashMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
                req.setCreatedAt(hashMap.get(id).getCreatedAt());
                req.setCreatedBy(hashMap.get(id).getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            res.add(req);
        }
        res = saveAll(res);
        for (var req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (oldEntityJsonStringMap.containsKey(req.getId())) {
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, ReferenceNumbers.class) : null)
                                .parent(ConsolidationDetails.class.getSimpleName())
                                .parentId(consolidationId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                     InvocationTargetException | NoSuchMethodException | RunnerException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    void deleteReferenceNumbers(Map<Long, ReferenceNumbers> hashMap, String entityType, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(referenceNumber -> {
                String json = jsonHelper.convertToJson(referenceNumber);
                delete(referenceNumber);
                if(entityType != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, ReferenceNumbers.class))
                                        .parent(entityType)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException |
                             InvocationTargetException | NoSuchMethodException | RunnerException e) {
                        log.error(e.getMessage());
                    }
                }
            });
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_DELETE_EXCEPTION_MSG;
            log.error(responseMsg, e);
        }
    }

    @Override
    public List<ReferenceNumbers> updateEntityFromShipment(List<ReferenceNumbers> referenceNumbersList, Long shipmentId, List<ReferenceNumbers> oldEntityList) throws RunnerException {
        String responseMsg;
        Map<UUID, ReferenceNumbers> referenceNumbersMap = new HashMap<>();
        if(oldEntityList != null && !oldEntityList.isEmpty()) {
            for (ReferenceNumbers entity:
                    oldEntityList) {
                referenceNumbersMap.put(entity.getGuid(), entity);
            }
        }

        List<ReferenceNumbers> responseReferenceNumbers = new ArrayList<>();
        try {
            ReferenceNumbers oldEntity;
            List<ReferenceNumbers> referenceNumbersRequests = new ArrayList<>();
            if (referenceNumbersList != null && !referenceNumbersList.isEmpty()) {
                for (ReferenceNumbers request : referenceNumbersList) {
                    oldEntity = referenceNumbersMap.get(request.getGuid());
                    if(oldEntity != null) {
                        referenceNumbersMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    referenceNumbersRequests.add(request);
                }
                responseReferenceNumbers = saveEntityFromShipment(referenceNumbersRequests, shipmentId);
            }
            Map<Long, ReferenceNumbers> hashMap = new HashMap<>();
            referenceNumbersMap.forEach((s, referenceNumbers) ->  hashMap.put(referenceNumbers.getId(), referenceNumbers));
            deleteReferenceNumbers(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseReferenceNumbers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }
}
