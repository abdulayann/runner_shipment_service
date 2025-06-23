package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.HblLockSettings;
import com.dpw.runner.shipment.services.repository.interfaces.IHblLockSettingsRepository;
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
class HblLockSettingsDaoTest {

    @InjectMocks
    private HblLockSettingsDao hblLockSettingsDao;

    @Mock
    private IHblLockSettingsRepository hblLockSettingsRepository;

    @Test
    void save() {
        HblLockSettings hblLockSettings = new HblLockSettings();
        when(hblLockSettingsRepository.save(any())).thenReturn(hblLockSettings);
        assertEquals(hblLockSettings, hblLockSettingsDao.save(hblLockSettings));
    }
}
