package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentServices;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IShipmentServicesDao extends JpaRepository<ShipmentServices, Long> {
    Page<ShipmentServices> findAll(Specification<ShipmentServices> spec, Pageable pageable);
}
