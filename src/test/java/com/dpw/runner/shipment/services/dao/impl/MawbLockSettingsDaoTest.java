package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.MawbLockSettings;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbLockSettingsRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class MawbLockSettingsDaoTest {

    @InjectMocks
    private MawbLockSettingsDao mawbLockSettingsDao;

    @Mock
    private IMawbLockSettingsRepository mawbLockSettingsRepository;

    @Test
    void save() {
        MawbLockSettings mawbLockSettings = new MawbLockSettings();
        when(mawbLockSettingsRepository.save(any())).thenReturn(mawbLockSettings);
        assertEquals(mawbLockSettings, mawbLockSettingsDao.save(mawbLockSettings));
    }
}
