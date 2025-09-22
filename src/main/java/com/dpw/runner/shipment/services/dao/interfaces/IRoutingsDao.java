package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public interface IRoutingsDao {
    Routings save(Routings routings);
    List<Routings> saveAll(List<Routings> routingsList);

    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);

    Optional<Routings> findById(Long id);

    Optional<Routings> findByGuid(UUID id);

    Optional<Routings> findByIdWithQuery(Long id);

    Optional<Routings> findByGuidWithQuery(UUID id);

    Page<Routings> findAllWithoutTenantFilter(Specification<Routings> spec, Pageable pageable);

    void delete(Routings routings);
    List<Routings> findByIdIn(List<Long> routingIds);
    void deleteByIdIn(List<Long> routingIds);

    List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws RunnerException;
    List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId, Map<Long, Routings> oldEntityMap);

    List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId);

    List<Routings> updateEntityFromBooking(List<Routings> routingsList, Long bookingId) throws RunnerException;

    List<Routings> saveEntityFromBooking(List<Routings> routings, Long bookingId);

    List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId) throws RunnerException;
    List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId, List<Routings> oldEntityList) throws RunnerException;

    List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId);
    List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId, Map<Long, Routings> oldEntityMap);

    List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId, List<Routings> oldEntityList) throws RunnerException;
    List<Routings> generateDefaultRouting(CarrierDetails carrierDetails, String transportMode);
    List<Routings> findRoutingsByConsolidationId(Long consolidationId);
    List<Routings> findByShipmentIdAndCarriage(Long shipmentId, RoutingCarriage routingCarriage);

    List<Routings> findByShipmentId(Long shipmentId);

    List<Routings> findByConsolidationId(Long consolidationId);

    void deleteAll(List<Routings> existingRoutingsForDeletion);

    void deleteAdditionalDataByRoutingsIdsConsolidationId(List<Long> routingsIds, Long consolidationId);

    void revertSoftDeleteByRoutingsIdsAndConsolidationId(List<Long> routingsIds, Long consolidationId);

    void deleteAdditionalDataByRoutingsIdsBookingId(List<Long> routingsIds, Long bookngId);

    void revertSoftDeleteByRoutingsIdsAndBookingId(List<Long> routingsIds, Long bookingId);

    void deleteAdditionalroutingsByShipmentId(List<Long> routingsIds, Long shipmentId);

    void revertSoftDeleteByroutingsIdsAndShipmentId(List<Long> routingsIds, Long shipmentId);
}
