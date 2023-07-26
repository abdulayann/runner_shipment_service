package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.Hbl;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface IAwbRepository extends JpaRepository<Awb, Long> {
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
    List<Awb> findByShipmentId(Long shipmentId);
}
