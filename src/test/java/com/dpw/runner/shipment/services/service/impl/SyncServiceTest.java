package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.Kafka.Producer.KafkaProducer;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.SyncConfig;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackContainerNumberChangeRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.PackingExcelModel;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataSyncResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IConsolidationService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.ISyncQueueService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.interfaces.IPackingSync;
import com.dpw.runner.shipment.services.utils.CSVParsingUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
//    @Mock
//    private RetryTemplate retryTemplate;

    @InjectMocks
    private SyncService syncService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

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
    void callSyncSuccess() throws RunnerException {
        String json = null;
        String id = "1";
        String guid = null;
        String entity = null;
        HttpHeaders headers = null;
        V1DataSyncResponse mockResponse = V1DataSyncResponse.builder().isSuccess(true).build();

        when(v1Service.v1DataSync(any(), any())).thenReturn(mockResponse);

        syncService.callSyncAsync(json, id, guid, entity, headers);
    }

    @Test
    void callSyncFailure() throws RunnerException, MessagingException, IOException {
        String json = null;
        String id = "1";
        String guid = null;
        String entity = null;
        HttpHeaders headers = null;
        V1DataSyncResponse mockResponse = V1DataSyncResponse.builder().isSuccess(false).build();

        // mock
        when(v1Service.v1DataSync(any(), any())).thenReturn(mockResponse);
        // call
        var e = assertThrows(RunnerException.class, () -> syncService.callSync(json, id, guid, entity, headers));
    }

    @Test
    void callSyncAsync() throws RunnerException {
        List<String> json = new ArrayList<>(Arrays.asList("1"));
        List<String> id = new ArrayList<>(Arrays.asList("1"));
        List<String> guid = new ArrayList<>(Arrays.asList("1"));
        List<String> entity = new ArrayList<>(Arrays.asList("1"));
        HttpHeaders headers = null;

        V1DataSyncResponse mockResponse = V1DataSyncResponse.builder().isSuccess(true).build();

        when(v1Service.v1DataSync(any(), any())).thenReturn(mockResponse);

        syncService.callSyncAsync(json, id, guid, entity, headers);
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
        syncService.pushToKafka(json, id, guid, entity, transactionId);
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
        syncService.pushToKafka(json, id, guid, entity, transactionId);
    }
}
