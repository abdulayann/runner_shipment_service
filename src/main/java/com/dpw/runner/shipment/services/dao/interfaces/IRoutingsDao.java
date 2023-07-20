package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface IRoutingsDao {
    Routings save(Routings routings);
    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);
    Optional<Routings> findById(Long id);
    void delete(Routings routings);
    List<Routings> updateEntityFromShipment(List<Routings> routingsList, Long shipmentId) throws Exception;
    Optional<Routings> findByGuid(UUID guid);
}
