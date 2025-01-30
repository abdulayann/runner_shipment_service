package com.dpw.runner.shipment.services.service.impl;


import com.dpw.api.quartz.service.QuartzJobService;
import com.dpw.messaging.api.response.QuartzJobResponse;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IQuartzJobInfoDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse.FileTransferConfigurations;
import com.dpw.runner.shipment.services.entity.QuartzJobInfo;
import com.dpw.runner.shipment.services.entity.enums.FileTransferCriteriaFields;
import com.dpw.runner.shipment.services.entity.enums.PrePostTrigger;
import com.dpw.runner.shipment.services.entity.enums.TimeUnit;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class QuartzJobInfoServiceTest {
    @Mock
    private IQuartzJobInfoDao quartzJobInfoDao;
    @Mock
    private QuartzJobService quartzJobService;
    @Mock
    private CommonUtils commonUtils;
    @InjectMocks
    private QuartzJobInfoService quartzJobInfoService;

    private QuartzJobInfo quartzJobInfo;

    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime ata;
    private LocalDateTime atd;

    private V1TenantSettingsResponse tenantSettingsResponse;
    private List<FileTransferConfigurations> fileTransferConfigurations;

    @BeforeEach
    void setUp() {
        quartzJobInfo = new QuartzJobInfo();
        quartzJobInfo.setId(1L);
        quartzJobInfo.setGuid(UUID.randomUUID());
        quartzJobInfo.setEntityId(100L);
        quartzJobInfo.setEntityType(Constants.SHIPMENT);
        quartzJobInfo.setStartTime(LocalDateTime.now().plusMinutes(10));

        eta = LocalDateTime.of(2024, 12, 1, 10, 0);
        etd = LocalDateTime.of(2024, 12, 1, 12, 0);
        ata = LocalDateTime.of(2024, 12, 1, 14, 0);
        atd = LocalDateTime.of(2024, 12, 1, 16, 0);

        tenantSettingsResponse = new V1TenantSettingsResponse();
        fileTransferConfigurations = new ArrayList<>();
        tenantSettingsResponse.setFileTransferConfigurations(fileTransferConfigurations);
    }

    @Test
    void testCreateSimpleJob_Success() {
        QuartzJobResponse mockResponse = new QuartzJobResponse();

        when(quartzJobService.scheduleOneTimeJob(any(), any(), any())).thenReturn(mockResponse);

        QuartzJobResponse actualResponse = quartzJobInfoService.createSimpleJob(quartzJobInfo);

        assertNotNull(actualResponse);
        assertEquals(mockResponse, actualResponse);

        verify(quartzJobService).scheduleOneTimeJob(any(), any(), any());
    }

    @Test
    void testCreateSimpleJob_NullResponse() {
        when(quartzJobService.scheduleOneTimeJob(any(), any(), any())).thenReturn(null);

        QuartzJobResponse actualResponse = quartzJobInfoService.createSimpleJob(quartzJobInfo);

        assertNull(actualResponse);

        verify(quartzJobService).scheduleOneTimeJob(any(), any(), any());
    }

    @Test
    void testCreateSimpleJob_Exception() {
        when(quartzJobService.scheduleOneTimeJob(any(), any(), any())).thenThrow(new RuntimeException("Mock Exception"));

        assertThrows(RuntimeException.class, () -> quartzJobInfoService.createSimpleJob(quartzJobInfo));

        verify(quartzJobService).scheduleOneTimeJob(any(), any(), any());
    }

    @Test
    void testUpdateSimpleJob_Success() {
        QuartzJobResponse mockResponse = new QuartzJobResponse();

        when(quartzJobService.updateOneTimeJob(anyString(), any())).thenReturn(mockResponse);

        QuartzJobResponse actualResponse = quartzJobInfoService.updateSimpleJob(quartzJobInfo);

        assertNotNull(actualResponse);
        assertEquals(mockResponse, actualResponse);

        verify(quartzJobService).updateOneTimeJob(anyString(), any());
    }

    @Test
    void testUpdateSimpleJob_NullResponse() {
        when(quartzJobService.updateOneTimeJob(anyString(), any())).thenReturn(null);

        QuartzJobResponse actualResponse = quartzJobInfoService.updateSimpleJob(quartzJobInfo);

        assertNull(actualResponse);

        verify(quartzJobService).updateOneTimeJob(anyString(), any());
    }

    @Test
    void testUpdateSimpleJob_Exception() {
        when(quartzJobService.updateOneTimeJob(anyString(), any())).thenThrow(new RuntimeException("Mock Exception"));

        assertThrows(RuntimeException.class, () -> quartzJobInfoService.updateSimpleJob(quartzJobInfo));

        verify(quartzJobService).updateOneTimeJob(anyString(), any());
    }

    @Test
    void testGetQuartzJobTime_NoActiveConfigurations() {
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

        LocalDateTime result = quartzJobInfoService.getQuartzJobTime(eta, etd, ata, atd, "SEA");

        assertNull(result);
        verify(commonUtils).getCurrentTenantSettings();
    }

    @Test
    void testGetQuartzJobTime_EmptyConfigurations() {
        tenantSettingsResponse.setFileTransferConfigurations(Collections.emptyList());
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

        LocalDateTime result = quartzJobInfoService.getQuartzJobTime(eta, etd, ata, atd, "SEA");

        assertNull(result);
        verify(commonUtils).getCurrentTenantSettings();
    }

    @Test
    void testGetQuartzJobTime_ValidConfigurations() {
        FileTransferConfigurations config1 = new FileTransferConfigurations();
        config1.setIsActive(1);
        config1.setTransportMode("AIR");
        config1.setCriteriaField(FileTransferCriteriaFields.ETA.getId());
        config1.setTriggerType(PrePostTrigger.PRE.getId());
        config1.setIntervalTime(2);
        config1.setIntervalTimeUnit(TimeUnit.HOUR.getId());

        FileTransferConfigurations config2 = new FileTransferConfigurations();
        config2.setIsActive(1);
        config2.setTransportMode("SEA");
        config2.setCriteriaField(FileTransferCriteriaFields.ETD.getId());
        config2.setTriggerType(PrePostTrigger.POST.getId());
        config2.setIntervalTime(1);
        config2.setIntervalTimeUnit(TimeUnit.DAY.getId());

        fileTransferConfigurations.add(config1);
        fileTransferConfigurations.add(config2);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

        LocalDateTime result = quartzJobInfoService.getQuartzJobTime(eta, etd, ata, atd, "AIR");

        assertNotNull(result);
        assertEquals(eta.minusHours(2), result);
        verify(commonUtils).getCurrentTenantSettings();
    }

    @Test
    void testGetQuartzJobTime_NullCriteriaFields() {
        FileTransferConfigurations config = new FileTransferConfigurations();
        config.setIsActive(1);
        config.setTransportMode("LAND");
        config.setCriteriaField(FileTransferCriteriaFields.ATA.getId());
        config.setTriggerType(PrePostTrigger.PRE.getId());
        config.setIntervalTime(3);
        config.setIntervalTimeUnit(TimeUnit.HOUR.getId());

        fileTransferConfigurations.add(config);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

        LocalDateTime result = quartzJobInfoService.getQuartzJobTime(eta, etd, null, atd, "AIR");

        assertNull(result);
        verify(commonUtils).getCurrentTenantSettings();
    }

    @Test
    void testGetQuartzJobTime_MultipleConfigurations() {
        FileTransferConfigurations config1 = new FileTransferConfigurations();
        config1.setIsActive(1);
        config1.setTransportMode("AIR");
        config1.setCriteriaField(FileTransferCriteriaFields.ETA.getId());
        config1.setTriggerType(PrePostTrigger.PRE.getId());
        config1.setIntervalTime(1);
        config1.setIntervalTimeUnit(TimeUnit.HOUR.getId());

        FileTransferConfigurations config2 = new FileTransferConfigurations();
        config2.setIsActive(1);
        config2.setTransportMode("SEA");
        config2.setCriteriaField(FileTransferCriteriaFields.ATD.getId());
        config2.setTriggerType(PrePostTrigger.POST.getId());
        config2.setIntervalTime(2);
        config2.setIntervalTimeUnit(TimeUnit.DAY.getId());

        fileTransferConfigurations.add(config1);
        fileTransferConfigurations.add(config2);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

        LocalDateTime result = quartzJobInfoService.getQuartzJobTime(eta, etd, ata, atd, "AIR");

        assertNotNull(result);
        assertEquals(eta.minusHours(1), result);
        verify(commonUtils).getCurrentTenantSettings();
    }

    @Test
    void testGetQuartzJobTime_DuplicateConfigurations() {
        FileTransferConfigurations config1 = new FileTransferConfigurations();
        config1.setIsActive(1);
        config1.setTransportMode("AIR");
        config1.setCriteriaField(FileTransferCriteriaFields.ETA.getId());
        config1.setTriggerType(PrePostTrigger.PRE.getId());
        config1.setIntervalTime(1);
        config1.setIntervalTimeUnit(TimeUnit.HOUR.getId());

        FileTransferConfigurations config2 = new FileTransferConfigurations();
        config2.setIsActive(1);
        config2.setTransportMode("AIR");
        config2.setCriteriaField(FileTransferCriteriaFields.ATD.getId());
        config2.setTriggerType(PrePostTrigger.POST.getId());
        config2.setIntervalTime(2);
        config2.setIntervalTimeUnit(TimeUnit.DAY.getId());

        fileTransferConfigurations.add(config1);
        fileTransferConfigurations.add(config2);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

        LocalDateTime result = quartzJobInfoService.getQuartzJobTime(eta, etd, ata, atd, "AIR");

        assertNotNull(result);
        assertEquals(eta.minusHours(1), result);
        verify(commonUtils).getCurrentTenantSettings();
    }

    @Test
    void testGetQuartzJobTime_NullTransportMode() {
        FileTransferConfigurations config = new FileTransferConfigurations();
        config.setIsActive(1);
        config.setTransportMode(null);
        config.setCriteriaField(FileTransferCriteriaFields.ETA.getId());
        config.setTriggerType(PrePostTrigger.PRE.getId());
        config.setIntervalTime(3);
        config.setIntervalTimeUnit(TimeUnit.HOUR.getId());

        fileTransferConfigurations.add(config);
        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettingsResponse);

        LocalDateTime result = quartzJobInfoService.getQuartzJobTime(eta, etd, ata, atd, "AIR");

        assertNull(result);
        verify(commonUtils).getCurrentTenantSettings();
    }

    @Test
    void testIsJobWithNamePresent() {
        when(quartzJobService.isJobWithNamePresent(anyString())).thenReturn(true);

        quartzJobInfoService.isJobWithNamePresent("Job");

        verify(quartzJobService, times(1)).isJobWithNamePresent(anyString());
    }
}
