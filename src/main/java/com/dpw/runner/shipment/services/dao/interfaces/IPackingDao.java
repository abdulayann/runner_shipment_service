package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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

    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Long> deleteContIds) throws RunnerException;

    List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId);
    List<Packing> saveEntityFromShipment(List<Packing> packings, Long shipmentId, Map<Long, Packing> oldEntityMap);

    List<Packing> saveEntityFromBooking(List<Packing> packings, Long bookindId);

    List<Packing> updateEntityFromBooking(List<Packing> packings, Long bookingId) throws RunnerException;

    List<Packing> updateEntityFromCarrierBooking(List<Packing> packings, Long carrierBookingId) throws RunnerException;

    List<Packing> updateEntityFromShippingInstruction(List<Packing> packings, Long shippingInstructionId) throws RunnerException;

    List<Packing> updateEntityFromConsoleWithCarrierBooking(List<Packing> packingList, Long consolidationId, Long carrierBookingId) throws RunnerException;
    List<Packing> updateEntityFromConsole(List<Packing> packingList, Long consolidationId, List<Packing> oldEntityList) throws RunnerException;

    List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId);
    List<Packing> saveEntityFromConsole(List<Packing> packings, Long consolidationId, Long carrierBookingId, Map<Long, Packing> oldEntityMap);

    List<Packing> getAllPackings();

    List<Packing> saveAll(List<Packing> packingList);

    List<Packing> saveEntityFromContainer(List<Packing> packings, Long containerId);

    void deleteEntityFromContainer(Long id);

    List<Packing> updateEntityFromShipment(List<Packing> packingList, Long shipmentId, List<Packing> oldEntityList, List<Packing> oldConsoleEntityList, List<Containers> containers, Map<UUID, String> map) throws RunnerException;

    List<Packing> findByConsolidationId(Long consolidationId);
}
