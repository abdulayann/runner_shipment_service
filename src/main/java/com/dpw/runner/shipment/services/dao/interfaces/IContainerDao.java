package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IContainerDao {
    Containers save(Containers containers);

    Page<Containers> findAll(Specification<Containers> spec, Pageable pageable);

    List<Containers> getAllContainers();

    Optional<Containers> findById(Long id);
    List<Containers> findByGuid(UUID guid);

    void delete(Containers containers);

    List<Containers> updateEntityFromShipmentConsole(List<Containers> containersList, Long consolidationId, Long shipmentId) throws Exception;

    List<Containers> updateEntityFromBooking(List<Containers> containersList, Long bookingId) throws Exception;

    List<Containers> saveAll(List<Containers> containers);

    List<Containers> updateEntityFromShipmentConsole(List<Containers> containersList, Long consolidationId, List<Containers> oldContainers) throws Exception;
    List<Containers> updateEntityFromShipmentV1(List<Containers> containersList, List<Containers> oldContainers) throws Exception;
    List<Containers> findByShipmentId(Long shipmentId);
    List<Containers> findByConsolidationId(Long shipmentId);
    void deleteById(Long id);

}
