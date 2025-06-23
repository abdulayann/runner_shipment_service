package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.HawbLockSettings;
import com.dpw.runner.shipment.services.repository.interfaces.IHawbLockSettingsRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class HawbLockSettingsDaoTest {

    @InjectMocks
    private HawbLockSettingsDao hawbLockSettingsDao;

    @Mock
    private IHawbLockSettingsRepository hawbLockSettingsRepository;

    @Test
    void save() {
        HawbLockSettings hawbLockSettings = new HawbLockSettings();
        when(hawbLockSettingsRepository.save(any())).thenReturn(hawbLockSettings);
        assertEquals(hawbLockSettings, hawbLockSettingsDao.save(hawbLockSettings));
    }
}
