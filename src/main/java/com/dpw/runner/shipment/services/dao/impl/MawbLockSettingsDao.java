package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.dao.interfaces.IMawbLockSettingsDao;
import com.dpw.runner.shipment.services.entity.MawbLockSettings;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbLockSettingsRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
@Slf4j
public class MawbLockSettingsDao implements IMawbLockSettingsDao {

    @Autowired
    private IMawbLockSettingsRepository mawbLockSettingsRepository;

    @Override
    public MawbLockSettings save(MawbLockSettings mawbLockSettings) {
        return mawbLockSettingsRepository.save(mawbLockSettings);
    }
}
