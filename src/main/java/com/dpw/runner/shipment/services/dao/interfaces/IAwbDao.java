package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.Awb;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Optional;

public interface IAwbDao {
    Awb save(Awb awbShipmentInfo);
    Page<Awb> findAll(Specification<Awb> spec, Pageable pageable);
    Optional<Awb> findById(Long id);
}
