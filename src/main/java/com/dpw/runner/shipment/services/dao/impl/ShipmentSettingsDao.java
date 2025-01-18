package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsRepository;
import com.dpw.runner.shipment.services.service.impl.CacheEvictionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
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

    @Autowired
    private CacheEvictionService cacheEvictionService;

    @Override
    public ShipmentSettingsDetails save(ShipmentSettingsDetails shipmentSetting) {
        if (shipmentSetting.getCancelledBLSuffix() != null && shipmentSetting.getCancelledBLSuffix().length() > 5) {
            throw new ValidationException("Maximum allowed characters is only upto 5 for cancelled bl suffix");
        }
        shipmentSetting = shipmentSettingsRepository.save(shipmentSetting);
        cacheEvictionService.clearCacheByName(CacheConstants.CACHE_KEY_USER, CacheConstants.SHIPMENT_SETTINGS + shipmentSetting.getTenantId());
        return shipmentSetting;
    }

    @Override
    public Optional<ShipmentSettingsDetails> findById(Long id) {
        return shipmentSettingsRepository.findById(id);
    }

    @Override
    public Optional<ShipmentSettingsDetails> findByGuid(UUID guid) {
        return shipmentSettingsRepository.findByGuid(guid);
    }

    @Override
    public Integer getShipmentConsoleImportApprovarRole(int tenantId) {
        return shipmentSettingsRepository.getShipmentConsoleImportApprovarRole(tenantId);
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

    @Override
    public Boolean getCustomisedSequence(Integer tenantId) {
        return shipmentSettingsRepository.getCustomisedSequence(tenantId);
    }

    @Override
    public List<ShipmentSettingsDetails> getSettingsByTenantIds(List<Integer> tenantId) {
        return shipmentSettingsRepository.getTenantSetting(tenantId);
    }

    @Override
    public Optional<ShipmentSettingsDetails> findByTenantId(Integer tenantId) {
        return shipmentSettingsRepository.findByTenantId(tenantId);
    }

    @Override
    @Cacheable(cacheNames = CacheConstants.CACHE_KEY_USER, keyGenerator = "customKeyGenerator")
    public Optional<ShipmentSettingsDetails> getSettingsByTenantIdWithCache(Integer tenantId) {
        return shipmentSettingsRepository.findByTenantId(tenantId);
    }
}