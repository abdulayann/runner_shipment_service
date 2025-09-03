package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.ProductSequenceConfigDto;
import com.dpw.runner.shipment.services.syncing.Entity.ShipmentSettingsSyncRequest;
import com.dpw.runner.shipment.services.syncing.Entity.TenantProductsDto;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.RestTemplate;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentSettingsSyncTest {
    @InjectMocks
    private ShipmentSettingsSync shipmentSettingsSync;

    @Mock
    private ModelMapper modelMapper;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private RestTemplate restTemplate;
    @Mock
    private IV1Service v1Service;
    @Mock
    private IShipmentSettingsDao shipmentSettingsDao;
    @Mock
    private V1AuthHelper v1AuthHelper;
    @Mock
    private EmailServiceUtility emailServiceUtility;
    @Mock
    private ISyncService syncService;

    @BeforeEach
    void setUp() {
        SyncingContext.setContext(Boolean.TRUE);
    }

    /**
     * Method under test: {@link ContainersSync#sync(List, Page)}
     */
    @Test
    void testSync() {
        var inputData = new ShipmentSettingsDetails();
        inputData.setHblLockSettings(new HblLockSettings());
        inputData.setHawbLockSettings(new HawbLockSettings());
        inputData.setMawbLockSettings(new MawbLockSettings());

        when(modelMapper.map(any(), eq(ShipmentSettingsSyncRequest.class))).thenReturn(new ShipmentSettingsSyncRequest());
        doNothing().when(syncService).pushToKafka(any(), any(), any(), any(), any());

        // Act
        var response = shipmentSettingsSync.sync(inputData);

        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void testSyncProductSequence() throws RunnerException {
        var inputData = new ProductSequenceConfig();

        when(modelMapper.map(any(), eq(ProductSequenceConfigDto.class))).thenReturn(new ProductSequenceConfigDto());
        when(modelMapper.map(any(), eq(TenantProductsDto.class))).thenReturn(new TenantProductsDto());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(10));

        // Act
        var response = shipmentSettingsSync.syncProductSequence(inputData, new HttpHeaders());

        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

    @Test
    void testSyncSettings() {

        TenantContext.setCurrentTenant(11);

        var inputData = new ShipmentSettingsDetails();
        inputData.setHblLockSettings(new HblLockSettings());
        inputData.setHawbLockSettings(new HawbLockSettings());
        inputData.setMawbLockSettings(new MawbLockSettings());

        when(modelMapper.map(any(), eq(ShipmentSettingsSyncRequest.class))).thenReturn(new ShipmentSettingsSyncRequest());
        doNothing().when(syncService).pushToKafka(any(), any(), any(), any(), any());

        when(shipmentSettingsDao.getSettingsByTenantIds(anyList())).thenReturn(List.of(inputData));
        // Act
        var response = shipmentSettingsSync.syncSettings();

        assertEquals(HttpStatus.OK, response.getStatusCode());

    }


}
