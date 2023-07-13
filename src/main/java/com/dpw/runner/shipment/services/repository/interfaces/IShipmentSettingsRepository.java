package com.dpw.runner.shipment.services.repository.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.apache.poi.hpsf.GUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;

import javax.swing.text.html.Option;
import java.util.Optional;
import java.util.UUID;

public interface IShipmentSettingsRepository extends JpaRepository<ShipmentSettingsDetails, Long> {
    Page<ShipmentSettingsDetails> findAll(Specification<ShipmentSettingsDetails> spec, Pageable pageable);
    Optional<ShipmentSettingsDetails> findOneByGuid(UUID guid);
}
