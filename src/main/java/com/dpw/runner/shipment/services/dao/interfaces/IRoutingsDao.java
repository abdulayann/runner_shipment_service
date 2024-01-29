package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Routings;
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

    void delete(Routings routings);

    List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws Exception;
    List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId, Map<Long, Routings> oldEntityMap);

    List<Routings> saveEntityFromShipment(List<Routings> routings, Long shipmentId);

    List<Routings> updateEntityFromBooking(List<Routings> routingsList, Long bookingId) throws Exception;

    List<Routings> saveEntityFromBooking(List<Routings> routings, Long bookingId);

    List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId) throws Exception;
    List<Routings> updateEntityFromConsole(List<Routings> routingsList, Long consolidationId, List<Routings> oldEntityList) throws Exception;

    List<Routings> saveEntityFromConsole(List<Routings> routings, Long consolidationId);

    List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId, List<Routings> oldEntityList) throws Exception;
}
