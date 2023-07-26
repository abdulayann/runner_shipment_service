package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Packing;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IPackingDao {
    Packing save(Packing packing);

    Page<Packing> findAll(Specification<Packing> spec, Pageable pageable);

    Optional<Packing> findById(Long id);

    void delete(Packing packing);

    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId) throws Exception;
    List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId);

    List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId) throws Exception;

    List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId);

    List<Packing> getAllPackings();

    List<Packing> saveAll(List<Packing> packingList);
    List<Packing> savePacks(List<Packing> packs, Long containerId);
    List<Packing> updateEntityFromContainer(List<Packing> packingList, Long containerId, List<Long> updatedPacksId) throws Exception;
    List<Packing> removeEntityFromContainer(List<Packing> packingList, Long containerId, List<Long> updatedPacksId) throws Exception;
    List<Packing> saveEntityFromContainer(List<Packing> packings, Long containerId);
    void deleteEntityFromContainer(Long id);
}
