package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.Routings;
import org.springframework.data.domain.Page;


import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IRoutingsRepository extends JpaRepository<Routings, Long> {
    Page<Routings> findAll(Specification<Routings> spec, Pageable pageable);

}
