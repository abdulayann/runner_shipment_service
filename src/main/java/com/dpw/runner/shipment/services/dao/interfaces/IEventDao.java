package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Events;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IEventDao {
    Events save(Events events);
    Page<Events> findAll(Specification<Events> spec, Pageable pageable);
    Optional<Events> findById(Long id);
    void delete(Events events);
    List<Events> updateEntityFromShipment(List<Events> eventsList, Long shipmentId) throws Exception;
}
