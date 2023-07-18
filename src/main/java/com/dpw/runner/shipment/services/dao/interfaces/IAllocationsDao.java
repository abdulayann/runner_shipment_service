package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Allocations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IAllocationsDao {
    Allocations save(Allocations allocations);
    Page<Allocations> findAll(Specification<Allocations> spec, Pageable pageable);
    Optional<Allocations> findById(Long id);
    void delete(Allocations allocations);
    Allocations updateEntityFromShipmentConsole(Allocations allocations) throws Exception;
}
