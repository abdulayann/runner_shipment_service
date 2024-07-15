package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.SyncQueue;
import com.dpw.runner.shipment.services.repository.interfaces.ISyncQueueRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.times;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ISyncQueueDaoTest {

    @InjectMocks
    private SyncQueueDao syncQueueDao;

    @Mock
    private ISyncQueueRepository syncQueueRepository;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void fetchDataByModuleTypeAndTenantIds() {
        List<Integer> tenantIds = Arrays.asList(1);
        List<SyncQueue> syncQueueList = Arrays.asList(SyncQueue.builder().build());
        when(syncQueueRepository.fetchByTenantIdsAndModuleType(any(), any())).thenReturn(syncQueueList);
        assertEquals(syncQueueList, syncQueueDao.fetchDataByModuleTypeAndTenantIds("entity", tenantIds));
    }

    @Test
    void fetchDataByTenantIds() {
        List<Integer> tenantIds = Arrays.asList(1);
        List<SyncQueue> syncQueueList = Arrays.asList(SyncQueue.builder().build());
        when(syncQueueRepository.fetchByTenantIds(any())).thenReturn(syncQueueList);
        assertEquals(syncQueueList, syncQueueDao.fetchDataByTenantIds(tenantIds));
    }

    @Test
    void save() {
        SyncQueue syncQueue = SyncQueue.builder().build();
        doNothing().when(syncQueueRepository).updateExistingDataInActive(any(), any(), any());
        when(syncQueueRepository.save(any())).thenReturn(syncQueue);
        assertEquals(syncQueue, syncQueueDao.save(syncQueue));
    }

    @Test
    void updateDataInActive() {
        syncQueueDao.updateDataInActive(Arrays.asList(1L));
        verify(syncQueueRepository, times(1)).updateDataInActiveByIds(any());
    }
}
