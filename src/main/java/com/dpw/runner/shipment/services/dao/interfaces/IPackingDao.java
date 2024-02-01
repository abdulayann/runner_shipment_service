package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public interface IPackingDao {
    Packing save(Packing packing);

    Page<Packing> findAll(Specification<Packing> spec, Pageable pageable);

    Optional<Packing> findById(Long id);

    Optional<Packing> findByGuid(UUID id);

    void delete(Packing packing);

    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Long> deleteContIds) throws Exception;

    List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId);
    List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId, Map<Long, Packing> oldEntityMap);

    List<Packing> saveEntityFromBooking(List<Packing> packings, Long bookindId);

    List<Packing> updateEntityFromBooking(List<Packing> packings, Long bookingId) throws Exception;

    List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId) throws Exception;
    List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId, List<Packing> oldEntityList) throws Exception;

    List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId);
    List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId, Map<Long, Packing> oldEntityMap);

    List<Packing> getAllPackings();

    List<Packing> saveAll(List<Packing> packingList);

    List<Packing> savePacks(List<Packing> packs, Long containerId);

    List<Packing> removeContainerFromPacking(List<Packing> packingList, Long containerId, List<Long> updatedPacksId) throws Exception;

    List<Packing> insertContainerInPacking(List<Packing> packingList, Long containerId) throws Exception;

    List<Packing> removeEntityFromContainer(List<Packing> packingList, Long containerId, List<Long> updatedPacksId) throws Exception;

    List<Packing> saveEntityFromContainer(List<Packing> packings, Long containerId);

    void deleteEntityFromContainer(Long id);

    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Packing> oldEntityList, List<Containers> containers, Map<UUID, String> map) throws Exception;
}
