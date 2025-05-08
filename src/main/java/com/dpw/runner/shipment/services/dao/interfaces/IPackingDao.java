package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.projection.PackingAssignmentProjection;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.*;

public interface IPackingDao {
    Packing save(Packing packing);

    Page<Packing> findAll(Specification<Packing> spec, Pageable pageable);

    Optional<Packing> findById(Long id);

    Optional<Packing> findByGuid(UUID id);

    void delete(Packing packing);

    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Long> deleteContIds) throws RunnerException;

    List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId);
    List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId, Map<Long, Packing> oldEntityMap);

    List<Packing> saveEntityFromBooking(List<Packing> packings, Long bookindId);

    List<Packing> updateEntityFromBooking(List<Packing> packings, Long bookingId) throws RunnerException;

    List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId) throws RunnerException;
    List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId, List<Packing> oldEntityList) throws RunnerException;

    List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId);
    List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId, Map<Long, Packing> oldEntityMap);

    List<Packing> getAllPackings();

    List<Packing> saveAll(List<Packing> packingList);

    List<Packing> saveEntityFromContainer(List<Packing> packings, Long containerId);

    void deleteEntityFromContainer(Long id);

    void removeContainersFromPacking(List<Long> containerIds);
    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Packing> oldEntityList, List<Packing> oldConsoleEntityList, Set<Containers> containers, Map<UUID, String> map) throws RunnerException;

    List<Packing> findByConsolidationId(Long consolidationId);
    List<Packing> findByContainerIdIn(List<Long> containerIds);

    List<Packing> findByContainerIdInWithoutTenantFilter(List<Long> containerIds);

    List<Packing> findByIdIn(List<Long> packingIds);

    void deleteByIdIn(List<Long> deletePackingIds);

    PackingAssignmentProjection getPackingAssignmentCountByShipment(Long shipmentId);

    List<Packing> findByShipmentId(Long shipmentId);

    Optional<Packing> findByIdWithQuery(Long id);

    Optional<Packing> findByGuidWithQuery(UUID guid);

    Page<Packing> findAllWithoutTenantFilter(Specification<Packing> spec, Pageable pageable);
}
