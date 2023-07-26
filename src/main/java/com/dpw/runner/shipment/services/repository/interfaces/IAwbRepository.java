package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.Awb;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IAwbRepository extends JpaRepository<Awb, Long> {
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
}
