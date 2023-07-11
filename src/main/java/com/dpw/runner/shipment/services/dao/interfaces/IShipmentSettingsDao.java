package com.dpw.runner.shipment.services.dao.interfaces;

import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;
import java.util.Optional;

public interface IShipmentSettingsDao {

    ShipmentSettingsDetails save(ShipmentSettingsDetails shipmentSetting);
    Optional<ShipmentSettingsDetails> findById(Long id);
    Page<ShipmentSettingsDetails> list(Specification<ShipmentSettingsDetails> spec, Pageable pageable);
    void delete(ShipmentSettingsDetails shipmentSetting);
    List<ShipmentSettingsDetails> list();
}
