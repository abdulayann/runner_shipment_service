package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class SyncServiceTest {

    @Mock
    private IV1Service v1Service;
    @Mock
    private EmailServiceUtility emailServiceUtility;
    @Mock
    private KafkaProducer producer;
    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private SyncService syncService;

    @BeforeEach
    void setUp() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(false).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void callSyncSuccess() {
        String json = null;
        String id = "1";
        String guid = null;
        String entity = null;
        HttpHeaders headers = null;
        V1DataSyncResponse mockResponse = V1DataSyncResponse.builder().isSuccess(true).build();

        when(v1Service.v1DataSync(any(), any())).thenReturn(mockResponse);

        assertDoesNotThrow(() -> syncService.callSyncAsync(json, id, guid, entity, headers));
    }

    @Test
    void callSyncFailure() {
        String json = null;
        String id = "1";
        String guid = null;
        String entity = null;
        HttpHeaders headers = null;
        V1DataSyncResponse mockResponse = V1DataSyncResponse.builder().isSuccess(false).build();

        // mock
        when(v1Service.v1DataSync(any(), any())).thenReturn(mockResponse);
        // call
        assertThrows(RunnerException.class, () -> syncService.callSync(json, id, guid, entity, headers));
    }

    @Test
    void callSyncAsync() {
        List<String> json = new ArrayList<>(List.of("1"));
        List<String> id = new ArrayList<>(List.of("1"));
        List<String> guid = new ArrayList<>(List.of("1"));
        List<String> entity = new ArrayList<>(List.of("1"));
        HttpHeaders headers = null;

        V1DataSyncResponse mockResponse = V1DataSyncResponse.builder().isSuccess(true).build();

        when(v1Service.v1DataSync(any(), any())).thenReturn(mockResponse);

        assertDoesNotThrow(() -> syncService.callSyncAsync(json, id, guid, entity, headers));
    }

    @Test
    void pushToKafkaFailure()
    {
        String json = null;
        String id = null;
        String guid = null;
        String entity = null;
        String transactionId = null;
        doThrow(new RuntimeException()).when(producer).produceToKafka(any(), any(), any());
        assertDoesNotThrow(() -> syncService.pushToKafka(json, id, guid, entity, transactionId));
    }

    @Test
    void pushToKafkaSuccess()
    {
        String json = null;
        String id = null;
        String guid = null;
        String entity = null;
        String transactionId = null;
        doNothing().when(producer).produceToKafka(any(), any(), any());
        assertDoesNotThrow(() -> syncService.pushToKafka(json, id, guid, entity, transactionId));
    }
}
