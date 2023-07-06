package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IShipmentSettingsRepository extends JpaRepository<ShipmentSettingsDetails, Long> {
    Page<ShipmentSettingsDetails> findAll(Specification<ShipmentSettingsDetails> spec, Pageable pageable);
}
