package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Containers;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IContainerDao {
    Containers save(Containers containers);
    Page<Containers> findAll(Specification<Containers> spec, Pageable pageable);
    Optional<Containers> findById(Long id);
    void delete(Containers containers);
}
