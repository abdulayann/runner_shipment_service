package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.ContainerConstants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentsContainersMappingRepository;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.nimbusds.jose.util.Pair;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

import static com.dpw.runner.shipment.services.helpers.DbAccessHelper.fetchData;
import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;

@Repository
@Slf4j
public class ShipmentsContainersMappingDao implements IShipmentsContainersMappingDao {

    @Autowired
    private IShipmentsContainersMappingRepository shipmentsContainersMappingRepository;

    private final IContainersSync containersSync;

    private final ExecutorService executorService;
    private final MasterDataUtils masterDataUtils;

    @Autowired
    public ShipmentsContainersMappingDao(IContainersSync containersSync, ExecutorService executorService,
        MasterDataUtils masterDataUtils) {
        this.containersSync = containersSync;
        this.executorService = executorService;
        this.masterDataUtils = masterDataUtils;
    }

    @Override
    public List<ShipmentsContainersMapping> findByContainerId(Long containerId) {
        return shipmentsContainersMappingRepository.findByContainerId(containerId);
    }

    @Override
    public List<ShipmentsContainersMapping> findByShipmentId(Long shipmentId) {
        return shipmentsContainersMappingRepository.findByShipmentId(shipmentId);
    }

    @Override
    public Page<ShipmentsContainersMapping> findAll(Specification<ShipmentsContainersMapping> spec, Pageable pageable) {
        return shipmentsContainersMappingRepository.findAll(spec, pageable);
    }

    @Override
    public Page<ShipmentsContainersMapping> findAllByContainerIds(List<Long> containerIds) {
        if(containerIds != null && !containerIds.isEmpty()) {
            ListCommonRequest listCommonRequest = constructListCommonRequest("containerId", containerIds, "IN");
            Pair<Specification<ShipmentsContainersMapping>, Pageable> pair = fetchData(listCommonRequest, ShipmentsContainersMapping.class);
            return findAll(pair.getLeft(), pair.getRight());
        }
        return null;
    }

    @Override
    public ShipmentsContainersMapping save(ShipmentsContainersMapping shipmentsContainersMapping) {
        return shipmentsContainersMappingRepository.save(shipmentsContainersMapping);
    }

    @Override
    public List<ShipmentsContainersMapping> saveAll(List<ShipmentsContainersMapping> shipmentsContainersMappingList) {
        return shipmentsContainersMappingRepository.saveAll(shipmentsContainersMappingList);
    }

    private void delete(ShipmentsContainersMapping shipmentsContainersMapping) {
        shipmentsContainersMappingRepository.delete(shipmentsContainersMapping);
    }
    private void deleteList(List<ShipmentsContainersMapping> shipmentsContainersMappings) {
        shipmentsContainersMappingRepository.deleteAll(shipmentsContainersMappings);
    }
    @Override
    public void assignContainers(Long shipmentId, List<Long> containerIds, String transactionId) {
        List<ShipmentsContainersMapping> mappings = findByShipmentId(shipmentId);
        HashSet<Long> contIds = new HashSet<>(containerIds);
        if (mappings != null && !mappings.isEmpty()) {
            for (ShipmentsContainersMapping shipmentsContainersMappings : mappings) {
                contIds.remove(shipmentsContainersMappings.getContainerId());
            }
        }
        if (!contIds.isEmpty()) {
            for (Long id : contIds) {
                ShipmentsContainersMapping entity = new ShipmentsContainersMapping();
                entity.setContainerId(id);
                entity.setShipmentId(shipmentId);
                save(entity);
            }
        }
        try {
            log.info("Call sync containers from assignContainers with ids: " + containerIds);
            containersSync.sync(containerIds, findAllByContainerIds(containerIds), transactionId);
        }
        catch (Exception e) {
            log.error("Error syncing shipment containers");
        }
    }

    @Override
    public void assignShipments(Long containerId, Set<Long> shipIds, boolean fromV1) {
        List<ShipmentsContainersMapping> mappings = findByContainerId(containerId);
        if (mappings != null && !mappings.isEmpty()) {
            for (ShipmentsContainersMapping shipmentsContainersMappings : mappings) {
                shipIds.remove(shipmentsContainersMappings.getShipmentId());
            }
        }
        if (shipIds!= null && !shipIds.isEmpty()) {
            for (Long id : shipIds) {
                ShipmentsContainersMapping entity = new ShipmentsContainersMapping();
                entity.setShipmentId(id);
                entity.setContainerId(containerId);
                save(entity);
            }
        }
        if(!fromV1) {
            try {
                log.info("Call sync containers from assignShipments with ids: " + containerId.toString());
                containersSync.sync(List.of(containerId), findAllByContainerIds(List.of(containerId)));
            }
            catch (Exception e) {
                log.error(ContainerConstants.ERROR_SYNCING_CONTAINERS);
            }
        }
    }

    @Override
    public void detachShipments(Long containerId, List<Long> shipIds, boolean fromV1) {
        List<ShipmentsContainersMapping> mappings = findByContainerId(containerId);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ShipmentsContainersMapping> deleteMappings = new ArrayList<>();
        if (mappings != null && !mappings.isEmpty()) {
            for (ShipmentsContainersMapping shipmentsContainersMappings : mappings) {
                if (shipmentIds.contains(shipmentsContainersMappings.getShipmentId())) {
                    deleteMappings.add(shipmentsContainersMappings);
                }
            }
        }
        if (!deleteMappings.isEmpty()) {
            for (ShipmentsContainersMapping shipmentsContainersMapping : deleteMappings) {
                delete(shipmentsContainersMapping);
            }
        }
        if(!fromV1) {
            try {
                log.info("Call sync containers from detachShipments with ids: " + containerId.toString());
                containersSync.sync(List.of(containerId), findAllByContainerIds(List.of(containerId)));
            }
            catch (Exception e) {
                log.error(ContainerConstants.ERROR_SYNCING_CONTAINERS);
            }
        }
    }
    public void detachListShipments(List<Long> containerIds, List<Long> shipIds, boolean fromV1) {
        List<ShipmentsContainersMapping> mappings = findByContainerIdIn(containerIds);
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ShipmentsContainersMapping> deleteMappings = new ArrayList<>();
        if (mappings != null && !mappings.isEmpty()) {
            for (ShipmentsContainersMapping shipmentsContainersMappings : mappings) {
                if (shipmentIds.contains(shipmentsContainersMappings.getShipmentId())) {
                    deleteMappings.add(shipmentsContainersMappings);
                }
            }
        }
        if (!deleteMappings.isEmpty()) {
            deleteList(deleteMappings);
        }
        if(!fromV1) {
            try {
                log.info("Call sync containers from detachShipments with ids: " + containerIds.toString());
                CompletableFuture.runAsync(masterDataUtils.withMdc(()-> containersSync.sync(containerIds, findAllByContainerIds(containerIds))), executorService);
            }
            catch (Exception e) {
                log.error(ContainerConstants.ERROR_SYNCING_CONTAINERS);
            }
        }
    }

    @Override
    public List<ShipmentsContainersMapping> findByContainerIdIn(List<Long> containerIds) {
        return shipmentsContainersMappingRepository.findByContainerIdIn(containerIds);
    }

    @Override
    public List<ShipmentsContainersMapping> findByContainerIdInWithoutTenantFilter(List<Long> containerIds) {
        return shipmentsContainersMappingRepository.findByContainerIdInWithoutTenantFilter(containerIds);
    }

    @Override
    public void updateShipmentsMappings(Long containerId, List<Long> shipIds) {
        List<ShipmentsContainersMapping> mappings = Optional.ofNullable(findByContainerId(containerId)).orElse(Collections.emptyList());
        HashSet<Long> shipmentIds = new HashSet<>(shipIds);
        List<ShipmentsContainersMapping> deleteMappings = new ArrayList<>();
        for (ShipmentsContainersMapping shipmentsContainersMapping : mappings) {
            if (shipmentIds.contains(shipmentsContainersMapping.getShipmentId())) {
                shipmentIds.remove(shipmentsContainersMapping.getShipmentId());
            } else {
                deleteMappings.add(shipmentsContainersMapping);
            }
        }
        if (!shipmentIds.isEmpty()) {
            for (Long shipmentId : shipmentIds) {
                ShipmentsContainersMapping entity = new ShipmentsContainersMapping();
                entity.setShipmentId(shipmentId);
                entity.setContainerId(containerId);
                save(entity);
            }
        }
        if (!deleteMappings.isEmpty()) {
            for (ShipmentsContainersMapping mapping : deleteMappings) {
                delete(mapping);
            }
        }
    }
}
