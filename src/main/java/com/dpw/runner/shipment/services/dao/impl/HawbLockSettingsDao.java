package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IHawbLockSettingsDao;
import com.dpw.runner.shipment.services.commons.entity.HawbLockSettings;
import com.dpw.runner.shipment.services.repository.interfaces.IHawbLockSettingsRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
@Slf4j
public class HawbLockSettingsDao implements IHawbLockSettingsDao {

    @Autowired
    private IHawbLockSettingsRepository hawbLockSettingsRepository;

    @Override
    public HawbLockSettings save(HawbLockSettings hawbLockSettings) {
        return hawbLockSettingsRepository.save(hawbLockSettings);
    }

}
