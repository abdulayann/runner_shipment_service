package com.dpw.runner.shipment.services.dao.impl;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static com.dpw.runner.shipment.services.utils.CommonUtils.isStringNullOrEmpty;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.mapper.ContainersMapper;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.entity.response.consolidation.IContainerLiteResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.projection.ContainerDeleteInfoProjection;
import com.dpw.runner.shipment.services.repository.interfaces.IContainerRepository;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingsSync;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.nimbusds.jose.util.Pair;
import java.lang.reflect.InvocationTargetException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;


@Repository
@Slf4j
public class ContainerDao implements IContainerDao {
    @Autowired
    private IContainerRepository containerRepository;

    @Autowired
    private ValidatorUtility validatorUtility;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAuditLogService auditLogService;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IPackingsSync packingsSync;

    @Autowired
    private IPackingDao packingDao;

    @Override
    public Containers save(Containers containers) {
        Set<String> errors = validatorUtility.applyValidation(jsonHelper.convertToJson(containers) , Constants.CONTAINER, LifecycleHooks.ON_CREATE, false);
        if(Boolean.TRUE.equals(containers.getHazardous()) && isStringNullOrEmpty(containers.getDgClass())) {
            errors.add("DG class is mandatory for Hazardous Goods Containers");
        }
        if (! errors.isEmpty())
            throw new ValidationException(String.join(",", errors));
        if(containers.getId() != null) {
            updateExistingContainerData(containers);
        }
        if(containers.getEventsList() != null && !containers.getEventsList().isEmpty()) {
            for (Events events : containers.getEventsList()) {
                events.setEntityType(Constants.CONTAINER);
            }
        }
        containers.setIsAttached(containers.getShipmentsList() != null && !containers.getShipmentsList().isEmpty());
        return containerRepository.save(containers);
    }

    private void updateExistingContainerData(Containers containers) {
        long id = containers.getId();
        Optional<Containers> oldEntity = findById(id);
        if (oldEntity.isEmpty()) {
            log.debug("Container is null for Id {} with Request Id {}", id, LoggerHelper.getRequestIdFromMDC());
            throw new DataRetrievalFailureException(DaoConstants.DAO_DATA_RETRIEVAL_FAILURE);
        }
        containers.setCreatedAt(oldEntity.get().getCreatedAt());
        containers.setCreatedBy(oldEntity.get().getCreatedBy());
        if(containers.getShipmentsList() == null) {
            containers.setShipmentsList(oldEntity.get().getShipmentsList());
        }
        if(containers.getEventsList() == null) {
            containers.setEventsList(oldEntity.get().getEventsList());
        }
        if(containers.getTruckingDetails() == null) {
            containers.setTruckingDetails(oldEntity.get().getTruckingDetails());
        }
    }

    @Override
    public Page<Containers> findAll(Specification<Containers> spec, Pageable pageable) {
        return containerRepository.findAll(spec, pageable);
    }
    public Page<Containers> findAllWithoutTenantFilter(Specification<Containers> spec, Pageable pageable){
        return containerRepository.findAllWithoutTenantFilter(spec, pageable);
    }

    @Override
    public List<IContainerLiteResponse> findAllLiteContainer(List<Long> consolidationId) {
        return containerRepository.findAllLiteContainer(consolidationId);
    }

    @Override
    public List<Containers> getAllContainers() {
        return containerRepository.findAll();
    }

    @Override
    public Optional<Containers> findById(Long id) {
        return containerRepository.findById(id);
    }

    @Override
    public List<Containers> findByGuid(UUID guid) {
        return containerRepository.findByGuid(guid);
    }

    @Override
    public void delete(Containers containers) {
        containerRepository.delete(containers);
    }

    @Override
    public void deleteAllById(List<Long> containerIdList) {
        containerRepository.deleteAllById(containerIdList);
    }

    private void deleteByIds(List<Long> ids) {
        if(ids != null && !ids.isEmpty()) {
            for (Long id: ids)
                deleteById(id);
        }
    }
    @Override
    public void deleteById(Long id) {
        containerRepository.deleteById(id);
    }

    public List<Containers> updateEntityFromBooking(List<Containers> containersList, Long bookingId) throws RunnerException {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        try {
            ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
            Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
            Page<Containers> containers = findAll(pair.getLeft(), pair.getRight());
            Map<Long, Containers> hashMap = containers.stream()
                    .collect(Collectors.toMap(Containers::getId, Function.identity()));
            List<Containers> containersRequestList = new ArrayList<>();
            if (containersList != null && !containersList.isEmpty()) {
                for (Containers request : containersList) {
                    Long id = request.getId();
                    if (id != null) {
                        hashMap.remove(id);
                    }
                    containersRequestList.add(request);
                }
                responseContainers = saveEntityFromBooking(containersRequestList, bookingId);
            }
            deleteContainers(hashMap, "CustomerBooking", bookingId);
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(responseMsg);
        }
    }

    public List<Containers> saveEntityFromBooking(List<Containers> containers, Long bookingId) {
        List<Containers> res = new ArrayList<>();
        ListCommonRequest listCommonRequest = constructListCommonRequest("bookingId", bookingId, "=");
        Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
        Page<Containers> containersPage = findAll(pair.getLeft(), pair.getRight());
        Map<Long, Containers> hashMap = containersPage.stream()
                .collect(Collectors.toMap(Containers::getId, Function.identity()));
        for (Containers req : containers) {
            String oldEntityJsonString = null;
            String operation = DBOperationType.CREATE.name();
            if (req.getId() != null) {
                long id = req.getId();
                if (hashMap.get(id) == null) {
                    log.debug("Containers is null for Id {}", req.getId());
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
                                .prevData(oldEntityJsonString != null ? jsonHelper.readFromJson(oldEntityJsonString, Containers.class) : null)
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

    private void deleteContainers(Map<Long, Containers> hashMap, String entity, Long entityId) {
        String responseMsg;
        try {
            hashMap.values().forEach(container -> {
                String json = jsonHelper.convertToJson(container);
                delete(container);
                if(entity != null)
                {
                    try {
                        auditLogService.addAuditLog(
                                AuditLogMetaData.builder()
                                .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                                        .newData(null)
                                        .prevData(jsonHelper.readFromJson(json, Containers.class))
                                        .parent(entity)
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
    public List<Containers> updateEntityFromShipmentConsole(List<Containers> containersList, Long consolidationId, Long shipmentId, boolean fromConsolidation) throws RunnerException {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        try {
            // LATER- Handle Transactions here
            if (containersList != null) {
                List<Containers> containerList = new ArrayList<>(containersList);
                if(fromConsolidation) {
                    processConsolidationContainers(consolidationId, containerList);
                }
                if(shipmentId != null)
                {
                    for (Containers container: containerList) {
                        container.setConsolidationId(consolidationId);
                        if (canSetAllocationDate(container))
                            container.setAllocationDate(LocalDateTime.now());
                        addAuditLogInShipmentConsole(shipmentId, container);
                    }
                }
                responseContainers = saveAll(containerList);
            }
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private boolean canSetAllocationDate(Containers container) {
        return Objects.isNull(container.getAllocationDate()) && !Objects.isNull(container.getContainerNumber());
    }

    private void addAuditLogInShipmentConsole(Long shipmentId, Containers container) throws RunnerException {
        String operation = DBOperationType.CREATE.name();
        Containers oldEntityJson = null;
        if(container.getId() != null)
        {
            Optional<Containers> oldEntity = this.findById(container.getId());
            if(oldEntity.isPresent())
            {
                operation = DBOperationType.UPDATE.name();
                oldEntityJson = ContainersMapper.INSTANCE.toContainers(oldEntity.get());
            }
        }
        try {
            auditLogService.addAuditLog(
                    AuditLogMetaData.builder()
                            .tenantId(UserContext.getUser().getTenantId()).userName(UserContext.getUser().Username)
                            .newData(container)
                            .prevData(oldEntityJson)
                            .parent(ShipmentDetails.class.getSimpleName())
                            .parentId(shipmentId)
                            .operation(operation).build()
            );
        } catch (IllegalAccessException | NoSuchFieldException | JsonProcessingException | InvocationTargetException | NoSuchMethodException e) {
            log.error(e.getMessage());
        }
    }

    private void processConsolidationContainers(Long consolidationId, List<Containers> containerList) {
        List<Containers> containersPage = findByConsolidationId(consolidationId);
        Map<Long, Containers> hashMap = containersPage.stream()
                    .collect(Collectors.toMap(Containers::getId, Function.identity()));
        for (Containers containers: containerList) {
            containers.setConsolidationId(consolidationId);
            Long id = containers.getId();
            if (Objects.isNull(containers.getAllocationDate()) && !Objects.isNull(containers.getContainerNumber()))
                containers.setAllocationDate(LocalDateTime.now());
            if (id != null) {
                hashMap.remove(id);
            }
        }
        deleteContainers(hashMap, null, null);
        processHashMap(hashMap);
    }

    private void processHashMap(Map<Long, Containers> hashMap) {
        if(!hashMap.isEmpty()) {
            List<Long> deletedContIds = hashMap.keySet().stream().toList();
            if(!deletedContIds.isEmpty()) {
                List<Packing> packings = packingDao.findByContainerIdIn(deletedContIds);
                if(!CollectionUtils.isEmpty(packings)) {
                    for (Packing packing : packings) {
                        packing.setContainerId(null);
                    }
                    packingDao.saveAll(packings);
                    try {
                        packingsSync.sync(packings, UUID.randomUUID().toString());
                    } catch (Exception e) {
                        log.error("Error performing sync on packings list, {}", e.getMessage());
                    }
                }
            }
        }
    }

    public List<Containers> saveAll(List<Containers> containers) {
        List<Containers> res = new ArrayList<>();
        for (Containers req : containers) {
            req = save(req);
            res.add(req);
        }
        return res;
    }

    public List<Containers> updateEntityFromConsolidationV1(List<Containers> containersList, Long consolidationId, List<Containers> oldEntityList) throws RunnerException {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        Map<UUID, Containers> containersMap = new HashMap<>();
        List<Long> deleteContIds = new ArrayList<>();
        processOldEntityList(oldEntityList, containersMap, deleteContIds);
        Containers oldContainer;
        try {
            // LATER- Handle Transactions here
            if (containersList != null && !containersList.isEmpty()) {
                List<Containers> containerList = new ArrayList<>(containersList);
                for (Containers containers: containerList) {
                    if(containersMap.containsKey(containers.getGuid())) {
                        oldContainer = containersMap.get(containers.getGuid());
                        containers.setId(oldContainer.getId());
                        deleteContIds.remove(oldContainer.getId());
                    } else {
                        containers.setId(null);
                    }
                    containers.setConsolidationId(consolidationId);
                }
                responseContainers = saveAll(containerList);
            }
            if(!deleteContIds.isEmpty()) {
                deleteByIds(deleteContIds);
            }
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    private void processOldEntityList(List<Containers> oldEntityList, Map<UUID, Containers> containersMap, List<Long> deleteContIds) {
        if(oldEntityList != null && !oldEntityList.isEmpty()) {
            for (Containers containers:
                    oldEntityList) {
                containersMap.put(containers.getGuid(), containers);
                deleteContIds.add(containers.getId());
            }
        }
    }

    @Override
    public List<Containers> updateEntityFromShipmentV1(List<Containers> containersList, List<Containers> oldEntityList) throws RunnerException {
        String responseMsg;
        List<Containers> responseContainers = new ArrayList<>();
        Map<UUID, Containers> containersMap = new HashMap<>();
        if(oldEntityList != null && !oldEntityList.isEmpty()) {
            for (Containers containers:
                    oldEntityList) {
                containersMap.put(containers.getGuid(), containers);
            }
        }
        Containers oldContainer;
        try {
            // LATER- Handle Transactions here
            if (containersList != null && !containersList.isEmpty()) {
                List<Containers> containerList = new ArrayList<>(containersList);
                for (Containers containers: containerList) {
                    if(containersMap.containsKey(containers.getGuid())) {
                        oldContainer = containersMap.get(containers.getGuid());
                        containers.setId(oldContainer.getId());
                    } else {
                        containers.setId(null);
                    }
                }
                responseContainers = saveAll(containerList);
            }
            return responseContainers;
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_FAILED_ENTITY_UPDATE;
            log.error(responseMsg, e);
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public List<Containers> findByShipmentId(Long shipmentId) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentsList", shipmentId, "CONTAINS");
        Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
        Page<Containers> containersPage = findAll(pair.getLeft(), pair.getRight());
        return containersPage.getContent();
    }

    @Override
    public List<Containers> findByShipmentIdWithoutTenantFilter(Long shipmentId) {
        ListCommonRequest listCommonRequest = constructListCommonRequest("shipmentsList", shipmentId, "CONTAINS");
        Pair<Specification<Containers>, Pageable> pair = fetchData(listCommonRequest, Containers.class);
        Page<Containers> containersPage = findAllWithoutTenantFilter(pair.getLeft(), pair.getRight());
        return containersPage.getContent();
    }

    @Override
    public List<Containers> findByConsolidationId(Long consolidationId) {
        return containerRepository.findByConsolidationId(consolidationId);
    }

    @Override
    public List<Containers> findByConsolidationIdWithoutTenantFilter(Long consolidationId) {
        return containerRepository.findByConsolidationIdWithoutTenantFilter(consolidationId);
    }

    @Override
    public List<Containers> findByConsolidationIdIn(List<Long> consolidationIds) {
        return containerRepository.findByConsolidationIdIn(consolidationIds);
    }

    @Override
    public List<Containers> findByBookingIdIn(List<Long> bookingIds) {
        return containerRepository.findByBookingIdIn(bookingIds);
    }
    @Override
    public List<Containers> findByIdIn(List<Long> containerIds) {
        return containerRepository.findByIdIn(containerIds);
    }

    @Override
    public void deleteByIdIn(List<Long> containerIds) {
        containerRepository.deleteAllById(containerIds);
    }

    @Override
    public List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToShipmentCargo(List<Long> containerIds) {
        return containerRepository.filterContainerIdsAttachedToShipmentCargo(containerIds);
    }

    @Override
    public List<ContainerDeleteInfoProjection> filterContainerIdsAttachedToPacking(List<Long> containerIds) {
        return containerRepository.filterContainerIdsAttachedToPacking(containerIds);
    }

    @Override
    public List<ContainerDeleteInfoProjection> findContainersAttachedToBothPackingAndCargo(List<Long> containerIds) {
        return containerRepository.findContainersAttachedToBothPackingAndCargo(containerIds);
    }

    @Override
    public List<Long> findContainerIdsAttachedToEitherPackingOrShipment(List<Long> containerIds) {
        return containerRepository.findContainerIdsAttachedToEitherPackingOrShipment(containerIds);
    }

}