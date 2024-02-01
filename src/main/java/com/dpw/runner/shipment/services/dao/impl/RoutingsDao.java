package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IRoutingsDao;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IRoutingsRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
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
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class RoutingsDao implements IRoutingsDao {
    @Autowired
    private IRoutingsRepository routingsRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public Routings save(Routings routings) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(routings), Constants.ROUTING, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(errors.toString());
        return routingsRepository.save(routings);
    }
    @Override
    public List<Routings> saveAll(List<Routings> routingsList) {
        for(var routings: routingsList) {
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(routings), Constants.ROUTING, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(errors.toString());
        }
        return routingsRepository.saveAll(routingsList);
    }

    @Override
    public Page<Routings> findAll(Specification<Routings> spec, Pageable pageable) {
        return routingsRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Routings> findById(Long id) {
        return routingsRepository.findById(id);
    }

    @Override
    public Optional<Routings> findByGuid(UUID id) {
        return routingsRepository.findByGuid(id);
    }

    @Override
    public void delete(Routings routings) {
        routingsRepository.delete(routings);
    }

    @Override
    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws Exception {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, Routings> hashMap = new HashMap<>();
            var routingsIdList = routingsList.stream().map(Routings::getId).toList();
            if(!Objects.isNull(routingsIdList) && !routingsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> routings = findAll(pair.getLeft(), pair.getRight());
                hashMap = routings.stream()
                        .collect(Collectors.toMap(Routings::getId, Function.identity()));
            }
            Map<Long, Routings> copyHashMap = new HashMap<>(hashMap);
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && routingsList.size() != 0) {
                for (Routings request : routingsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromShipment(routingsRequestList, shipmentId, copyHashMap);
            }
            deleteRoutings(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId) {
        List<Routings> res = new ArrayList<>();
        for (Routings req : routings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Routings> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Routing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
                oldEntityJsonString = jsonHelper.convertToJson(oldEntity.get());
                operation = DBOperationType.UPDATE.name();
            }
            req.setShipmentId(shipmentId);
            req = save(req);
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Routings.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }
    @Override
    public List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId, Map<Long, Routings> oldEntityMap) {
        List<Routings> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for (Routings req : routings) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Routing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
                String oldEntityJsonString = jsonHelper.convertToJson(oldEntityMap.get(id));
                oldEntityJsonStringMap.put(id, oldEntityJsonString);
            }
            req.setShipmentId(shipmentId);
            res.add(req);
        }
        res = saveAll(res);
        for (Routings req : res) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if(oldEntityJsonStringMap.containsKey(req.getId())){
                oldEntityJsonString = oldEntityJsonStringMap.get(req.getId());
                operation = DBOperationType.UPDATE.name();
            }
            try {
                auditLogService.addAuditLog(
                        AuditLogMetaData.builder()
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Routings.class) : null)
                                .parent(ShipmentDetails.class.getSimpleName())
                                .parentId(shipmentId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
                log.error(e.getMessage());
            }
        }
        return res;
    }

    @Override
    public List<Routings> updateEntityFromBooking(List<Routings> routingsList, Long bookingId) throws Exception {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
            Page<Routings> routings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Routings> hashMap = routings.stream()
                    .collect(Collectors.toMap(Routings::getId, Function.identity()));
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && routingsList.size() != 0) {
                for (Routings request : routingsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromBooking(routingsRequestList, bookingId);
            }
            deleteRoutings(hashMap, "CustomerBooking", bookingId);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Routings> saveEntityFromBooking(List<Routings> routings, Long bookingId) {
        List<Routings> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
        Page<Routings> routingsPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, Routings> hashMap = routingsPage.stream()
                .collect(Collectors.toMap(Routings::getId, Function.identity()));
        for (Routings req : routings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug("Routing is null for Id {}", req.getId());
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
                                .newData(req)
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Routings.class) : null)
                                .parent(CustomerBooking.class.getSimpleName())
                                .parentId(bookingId)
                                .operation(operation).build()
                );
            } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
                log.error(e.getMessage());
            }
            res.add(req);
        }
        return res;
    }

    public List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId) throws Exception {
        String responseMsg;
        List<Routings> responseRoutings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, Routings> hashMap = new HashMap<>();
            var routingsIdList = routingsList.stream().map(Routings::getId).toList();
            if(!Objects.isNull(routingsIdList) && !routingsIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
                Pair<Specification<Routings>, Pageable> pair = fetchData(listCommonRequest, Routings.class);
                Page<Routings> routings = findAll(pair.getLeft(), pair.getRight());
                hashMap = routings.stream()
                        .collect(Collectors.toMap(Routings::getId, Function.identity()));
            }
            Map<Long, Routings> copyHashMap = new HashMap<>(hashMap);
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && routingsList.size() != 0) {
                for (Routings request : routingsList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromConsole(routingsRequestList, consolidationId, copyHashMap);
            }
            deleteRoutings(hashMap, null, null);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId, List<Routings> oldEntityList) throws Exception {
        String responseMsg;
        Map<UUID, Routings> routingMap = new HashMap<>();
        if (oldEntityList != null && oldEntityList.size() > 0) {
            for (Routings entity :
                    oldEntityList) {
                routingMap.put(entity.getGuid(), entity);
            }
        }

        List<Routings> responseRoutings = new ArrayList<>();
        try {
            Routings oldEntity;
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && routingsList.size() != 0) {
                for (Routings request : routingsList) {
                    oldEntity = routingMap.get(request.getGuid());
                    if (oldEntity != null) {
                        routingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromConsole(routingsRequestList, consolidationId);
            }
            Map<Long, Routings> hashMap = new HashMap<>();
            routingMap.forEach((s, routings) -> hashMap.put(routings.getId(), routings));
            deleteRoutings(hashMap, null, null);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId) {
        List<Routings> res = new ArrayList<>();
        for (Routings req : routings) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Routings> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Routing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntity.get().getCreatedAt());
                req.setCreatedBy(oldEntity.get().getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            req = save(req);
            res.add(req);
        }
        return res;
    }
    @Override
    public List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId, Map<Long, Routings> oldEntityMap) {
        List<Routings> res = new ArrayList<>();
        for (Routings req : routings) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Routing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
                req.setCreatedAt(oldEntityMap.get(id).getCreatedAt());
                req.setCreatedBy(oldEntityMap.get(id).getCreatedBy());
            }
            req.setConsolidationId(consolidationId);
            res.add(req);
        }
        res = saveAll(res);
        return res;
    }

    private void deleteRoutings(Map<Long, Routings> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(routing -> {
                String json = jsonHelper.convertToJson(routing);
                delete(routing);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Routings.class))
                                        .parent(entity)
                                        .parentId(entityId)
                                        .operation(DBOperationType.DELETE.name()).build()
                        );
                    } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
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
    public List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId, List<Routings> oldEntityList) throws Exception {
        String responseMsg;
        Map<UUID, Routings> routingMap = new HashMap<>();
        if (oldEntityList != null && oldEntityList.size() > 0) {
            for (Routings entity :
                    oldEntityList) {
                routingMap.put(entity.getGuid(), entity);
            }
        }

        List<Routings> responseRoutings = new ArrayList<>();
        try {
            Routings oldEntity;
            List<Routings> routingsRequestList = new ArrayList<>();
            if (routingsList != null && routingsList.size() != 0) {
                for (Routings request : routingsList) {
                    oldEntity = routingMap.get(request.getGuid());
                    if (oldEntity != null) {
                        routingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    routingsRequestList.add(request);
                }
                responseRoutings = saveEntityFromShipment(routingsRequestList, shipmentId);
            }
            Map<Long, Routings> hashMap = new HashMap<>();
            routingMap.forEach((s, routings) -> hashMap.put(routings.getId(), routings));
            deleteRoutings(hashMap, null, null);
            return responseRoutings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
