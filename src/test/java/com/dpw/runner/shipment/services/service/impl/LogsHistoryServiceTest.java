package com.dpw.runner.shipment.services.service.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.ILogsHistoryDao;
import com.dpw.runner.shipment.services.dto.request.LogHistoryRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.LogHistoryResponse;
import com.dpw.runner.shipment.services.entity.LogsHistory;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.JsonCompression;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Base64;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class LogsHistoryServiceTest {
    @Mock
    private ILogsHistoryDao logsHistoryDao;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private LogsHistoryService logsHistoryService;
    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @Test
    void testCreateLogHistory_Success() {
        LogHistoryRequest logHistoryRequest = LogHistoryRequest.builder().entityId(1L).entityType(Constants.SHIPMENT).entityPayload("ShipmentPayload").build();
        logsHistoryService.createLogHistory(logHistoryRequest);
        verify(logsHistoryDao, times(1)).save(any());
    }

    @Test
    void testCreateLogHistory_Exception() {
        LogHistoryRequest logHistoryRequest = LogHistoryRequest.builder().entityId(1L).entityType(Constants.SHIPMENT).entityPayload("ShipmentPayload").build();
        when(logsHistoryDao.save(any())).thenThrow(new RuntimeException(""));
        logsHistoryService.createLogHistory(logHistoryRequest);
        verify(logsHistoryDao, times(1)).save(any());
    }

    @Test
    void testRetrieveByEntityGuid_Success() throws RunnerException, IOException {
        UUID entityGuid = UUID.randomUUID();
        LocalDateTime time = LocalDateTime.now();
        LogsHistory logsHistory = LogsHistory.builder().entityId(1L).entityGuid(entityGuid).entityType(Constants.SHIPMENT).entityPayload(Base64.getEncoder().encodeToString(JsonCompression.compressJson("ShipmentPayload"))).build();
        when(logsHistoryDao.findByEntityGuidAndTimeStamp(entityGuid, time)).thenReturn(Optional.of(logsHistory));
        LogHistoryResponse response = logsHistoryService.findByEntityGuidAndTimeStamp(entityGuid, time);
        assertEquals(response.getEntityGuid(), entityGuid);
    }

    @Test
    void testRetrieveByEntityGuid_DataRetrievalException(){
        UUID entityGuid = UUID.randomUUID();
        LocalDateTime time = LocalDateTime.now();
        when(logsHistoryDao.findByEntityGuidAndTimeStamp(entityGuid, time)).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> logsHistoryService.findByEntityGuidAndTimeStamp(entityGuid, time));
    }

    @Test
    void testFindByEntityGuidsAndTimeStamp_Success() throws RunnerException, IOException {
        UUID entityGuid = UUID.randomUUID();
        LocalDateTime time = LocalDateTime.now();
        LogsHistory logsHistory = LogsHistory.builder().entityId(1L).entityGuid(entityGuid).entityType(Constants.SHIPMENT).entityPayload(Base64.getEncoder().encodeToString(JsonCompression.compressJson("ShipmentPayload"))).build();
        when(logsHistoryDao.findByEntityGuidsAndTimeStamp(List.of(entityGuid), time)).thenReturn(List.of(logsHistory));
        List<LogHistoryResponse> response = logsHistoryService.findByEntityGuidsAndTimeStamp(List.of(entityGuid), time);
        assertEquals(response.get(0).getEntityGuid(), entityGuid);
    }
}
