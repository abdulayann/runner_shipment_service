package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IHblLockSettingsDao;
import com.dpw.runner.shipment.services.entity.HblLockSettings;
import com.dpw.runner.shipment.services.repository.interfaces.IHblLockSettingsRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
@Slf4j
public class HblLockSettingsDao implements IHblLockSettingsDao {

    @Autowired
    private IHblLockSettingsRepository hblLockSettingsRepository;

    @Override
    public HblLockSettings save(HblLockSettings hblLockSettings) {
        return hblLockSettingsRepository.save(hblLockSettings);
    }
}
