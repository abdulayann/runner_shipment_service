package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Routings;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IRoutingsDao {
    Routings save(Routings routings);
    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);
    Optional<Routings> findById(Long id);
    void delete(Routings routings);
}
