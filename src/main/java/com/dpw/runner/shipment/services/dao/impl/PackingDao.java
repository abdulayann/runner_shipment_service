package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IPackingRepository;
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
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class PackingDao implements IPackingDao {
    @Autowired
    private IPackingRepository packingRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Override
    public Packing save(Packing packing) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(packing), Constants.PACKING, LifecycleHooks.ON_CREATE, false);
        if (!errors.isEmpty())
            throw new ValidationException(errors.toString());
        return packingRepository.save(packing);
    }

    @Override
    public Page<Packing> findAll(Specification<Packing> spec, Pageable pageable) {
        return packingRepository.findAll(spec, pageable);
    }

    @Override
    public Optional<Packing> findById(Long id) {
        return packingRepository.findById(id);
    }

    @Override
    public Optional<Packing> findByGuid(UUID id) {
        return packingRepository.findByGuid(id);
    }

    @Override
    public void delete(Packing packing) {
        packingRepository.delete(packing);
    }

    public List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Long> deleteContIds) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, Packing> hashMap = new HashMap<>();
            var packIdList = packingList.stream().map(Packing::getId).toList();
            if(!Objects.isNull(packIdList) && !packIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentId", shipmentId, "=");
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
                hashMap = packings.stream()
                        .collect(Collectors.toMap(Packing::getId, Function.identity()));
            }
            Map<Long, Packing> hashMapCopy = new HashMap<>(hashMap);
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    if(deleteContIds != null && request.getContainerId() != null && deleteContIds.contains(request.getContainerId()))
                        request.setContainerId(null);
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromShipment(packingRequestList, shipmentId, hashMapCopy);
            }
            deletePackings(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Packing> updateEntityFromBooking(List<Packing> packingList, Long bookingId) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Packing> hashMap = packings.stream()
                    .collect(Collectors.toMap(Packing::getId, Function.identity()));
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromBooking(packingRequestList, bookingId);
            }
            deletePackings(hashMap, "CustomerBooking", bookingId);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            Map<Long, Packing> hashMap = new HashMap<>();
            var packIdList = packingList.stream().map(Packing::getId).toList();
            if(!Objects.isNull(packIdList) && !packIdList.isEmpty()) {
                ListCommonRequest listCommonRequest = constructListCommonRequest("consolidationId", consolidationId, "=");
                Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
                Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
                hashMap = packings.stream()
                        .collect(Collectors.toMap(Packing::getId, Function.identity()));
            }
            Map<Long, Packing> hashMapCopy = new HashMap<>(hashMap);
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromConsole(packingRequestList, consolidationId, hashMapCopy);
            }
            deletePackings(hashMap, null, null);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId, List<Packing> oldEntityList) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        Map<UUID, Packing> packingMap = new HashMap<>();
        if (oldEntityList != null && oldEntityList.size() > 0) {
            for (Packing entity :
                    oldEntityList) {
                packingMap.put(entity.getGuid(), entity);
            }
        }
        try {
            Packing oldEntity;
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    oldEntity = packingMap.get(request.getGuid());
                    if (oldEntity != null) {
                        packingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromConsole(packingRequestList, consolidationId);
            }
            Map<Long, Packing> hashMap = new HashMap<>();
            packingMap.forEach((s, packing) -> hashMap.put(packing.getId(), packing));

            deletePackings(hashMap, null, null);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    @Override
    public List<Packing> getAllPackings() {
        return packingRepository.findAll();
    }

    @Override
    public List<Packing> saveAll(List<Packing> packingList) {
        for(var packing : packingList){
            Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(packing), Constants.PACKING, LifecycleHooks.ON_CREATE, false);
            if (!errors.isEmpty())
                throw new ValidationException(errors.toString());
        }
        return packingRepository.saveAll(packingList);
    }

    public List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
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
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Packing.class) : null)
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
    public List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId, Map<Long, Packing> oldEntityMap) {
        List<Packing> res = new ArrayList<>();
        Map<Long, String> oldEntityJsonStringMap = new HashMap<>();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Packing is null for Id {}", req.getId());
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
        for (var req : res) {
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
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Packing.class) : null)
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

    public List<Packing> saveEntityFromBooking(List<Packing> packings, Long bookingId) {
        List<Packing> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
        Page<Packing> packingPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, Packing> hashMap = packingPage.stream()
                .collect(Collectors.toMap(Packing::getId, Function.identity()));
        for (Packing req : packings) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug("Packing is null for Id {}", req.getId());
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
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Packing.class) : null)
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

    @Override
    public List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
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
    public List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId, Map<Long, Packing> oldEntityMap) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                if (!oldEntityMap.containsKey(id)) {
                    log.debug("Packing is null for Id {}", req.getId());
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

    private void deletePackings(Map<Long, Packing> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(packing -> {
                String json = jsonHelper.convertToJson(packing);
                delete(packing);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Packing.class))
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

    public List<Packing> savePacks(List<Packing> packs, Long contianerId) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packs) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Container is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setContainerId(contianerId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public List<Packing> removeContainerFromPacking(List<Packing> packingList, Long containerId, List<Long> updatedPacksId) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        try {
            // TODO- Handle Transactions here
            ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", containerId, "=");
            Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
            Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
            removeEntityFromContainer(packings.getContent(), null, updatedPacksId);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }

    public List<Packing> insertContainerInPacking(List<Packing> packings, Long containerId) throws Exception {
        List<Packing> res = new ArrayList<>();
        Optional<Packing> oldEntity = Optional.empty();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            if (oldEntity.isPresent() && oldEntity.get().getContainerId() != containerId) {
                req = oldEntity.get();
                req.setContainerId(containerId);
                req = save(req);
                res.add(req);
            }
        }
        return res;
    }

    public List<Packing> saveEntityFromContainer(List<Packing> packings, Long containerId) {
        List<Packing> res = new ArrayList<>();
        for (Packing req : packings) {
            if (req.getId() != null) {
                long id = req.getId();
                Optional<Packing> oldEntity = findById(id);
                if (!oldEntity.isPresent()) {
                    log.debug("Packing is null for Id {}", req.getId());
                    throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                }
            }
            req.setContainerId(containerId);
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public List<Packing> removeEntityFromContainer(List<Packing> packings, Long containerId, List<Long> updatedPacksId) {
        List<Packing> res = new ArrayList<>();
        HashSet<Long> remaniningPacksId = new HashSet<>();
        for (Long packId : updatedPacksId) {
            remaniningPacksId.add(packId);
        }
        for (Packing req : packings) {
            if (!remaniningPacksId.contains(req.getId())) {
                if (req.getId() != null) {
                    long id = req.getId();
                    Optional<Packing> oldEntity = findById(id);
                    if (!oldEntity.isPresent()) {
                        log.debug("Packing is null for Id {}", req.getId());
                        throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
                    }
                }
                req.setContainerId(null);
                req = save(req);
                res.add(req);
            }
        }
        return res;
    }

    public void deleteEntityFromContainer(Long containerId) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", containerId, "=");
        Pair<Specification<Packing>, Pageable> pair = fetchData(listCommonRequest, Packing.class);
        Page<Packing> packings = findAll(pair.getLeft(), pair.getRight());
        saveEntityFromContainer(packings.getContent(), null);
    }

    public List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Packing> oldEntityList, List<Containers> containers, Map<UUID, String> packMap) throws Exception {
        String responseMsg;
        List<Packing> responsePackings = new ArrayList<>();
        Map<UUID, Packing> packingMap = new HashMap<>();
        if (oldEntityList != null && oldEntityList.size() > 0) {
            for (Packing entity :
                    oldEntityList) {
                packingMap.put(entity.getGuid(), entity);
            }
        }
        try {
            Packing oldEntity;
            Map<String, Long> contMap = new HashMap<>();
            if(containers != null) {
                contMap = containers.stream().collect(Collectors.toMap(Containers::getContainerNumber, Containers::getId));
            }
            List<Packing> packingRequestList = new ArrayList<>();
            if (packingList != null && packingList.size() != 0) {
                for (Packing request : packingList) {
                    oldEntity = packingMap.get(request.getGuid());
                    if (oldEntity != null) {
                        packingMap.remove(oldEntity.getGuid());
                        request.setId(oldEntity.getId());
                    }
                    else {
                        Optional<Packing> oldPackCont = findByGuid(request.getGuid());
                        if(oldPackCont != null && oldPackCont.isPresent()) {
                            request.setId(oldPackCont.get().getId());
                        }
                    }
                    if(packMap.containsKey(request.getGuid()) && !IsStringNullOrEmpty(packMap.get(request.getGuid())) && contMap.containsKey(packMap.get(request.getGuid())))
                        request.setContainerId(contMap.get(packMap.get(request.getGuid())));
                    packingRequestList.add(request);
                }
                responsePackings = saveEntityFromShipment(packingRequestList, shipmentId);
            }
            Map<Long, Packing> hashMap = new HashMap<>();
            packingMap.forEach((s, packing) -> hashMap.put(packing.getId(), packing));

            deletePackings(hashMap, ShipmentDetails.class.getSimpleName(), shipmentId);
            return responsePackings;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new Exception(e);
        }
    }
}
