package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsRepository;
import org.apache.poi.hpsf.GUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public class ShipmentSettingsDao implements IShipmentSettingsDao {

    @Autowired
    private IShipmentSettingsRepository shipmentSettingsRepository;

    @Override
    public ShipmentSettingsDetails save(ShipmentSettingsDetails shipmentSetting) {
        return shipmentSettingsRepository.save(shipmentSetting);
    }

    @Override
    public Optional<ShipmentSettingsDetails> findById(Long id) {
        return shipmentSettingsRepository.findById(id);
    }

    @Override
    public Optional<ShipmentSettingsDetails> findByGuid(UUID guid) {
        return shipmentSettingsRepository.findOneByGuid(guid);
    }

    @Override
    public Page<ShipmentSettingsDetails> list(Specification<ShipmentSettingsDetails> spec, Pageable pageable) {
        return shipmentSettingsRepository.findAll(spec, pageable);
    }

    @Override
    public void delete(ShipmentSettingsDetails shipmentSetting) {
        shipmentSettingsRepository.delete(shipmentSetting);
    }

    @Override
    public List<ShipmentSettingsDetails> list() {
        return shipmentSettingsRepository.findAll();
    }
}