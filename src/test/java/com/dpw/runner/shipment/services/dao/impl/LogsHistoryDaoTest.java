package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.LogsHistory;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.ILogsHistoryRepository;
import com.dpw.runner.shipment.services.utils.JsonCompression;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class LogsHistoryDaoTest {
    @Mock
    private ILogsHistoryRepository logsHistoryRepository;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private LogsHistoryDao logsHistoryDao;

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
    }

    @Test
    void testSave() {
        LogsHistory logsHistory = new LogsHistory();
        Mockito.when(logsHistoryRepository.save(Mockito.any())).thenReturn(logsHistory);
        LogsHistory logsHistory1 = logsHistoryDao.save(Mockito.any());
        assertEquals(logsHistory, logsHistory1);
    }

    @Test
    void testSaveAll() {
        List<LogsHistory> logsHistory = new ArrayList<>();
        Mockito.when(logsHistoryRepository.saveAll(Mockito.any())).thenReturn(logsHistory);
        List<LogsHistory> logsHistory1 = logsHistoryDao.saveAll(logsHistory);
        assertEquals(logsHistory1.size(), logsHistory.size());
    }

    @Test
    void testFindAllWithSpec() {
        Specification<LogsHistory> spec = null;
        Pageable pageable = null;
        List<LogsHistory> logsHistoryList = new ArrayList<>();
        Page<LogsHistory> logsHistories = new PageImpl<>(logsHistoryList);
        Mockito.when(logsHistoryRepository.findAll(spec, pageable)).thenReturn(logsHistories);
        Page<LogsHistory> logsHistories1 = logsHistoryDao.findAll(spec, pageable);
        assertEquals(logsHistories.getTotalElements(), logsHistories1.getTotalElements());
    }

    @Test
    void findById() {
        LogsHistory logsHistory = new LogsHistory();
        logsHistory.setId(1L);
        Long id = 1L;
        Mockito.when(logsHistoryRepository.findById(Mockito.any())).thenReturn(Optional.of(logsHistory));
        Optional<LogsHistory> logsHistory1 = logsHistoryDao.findById(id);
        assertTrue(logsHistory1.isPresent());
        assertEquals(logsHistory.getId(), logsHistory1.get().getId());
    }

    @Test
    void testDelete() {
        LogsHistory logsHistory = new LogsHistory();
        logsHistoryDao.delete(logsHistory);
        verify(logsHistoryRepository, times(1)).delete(any());
    }

    @Test
    void testFindByEntityGuidAndTimeStamp() throws IOException {
        UUID entityGuid = UUID.randomUUID();
        LocalDateTime timeStamp = LocalDateTime.now();
        LogsHistory logsHistory = LogsHistory.builder().entityId(1L).entityGuid(entityGuid).entityType(Constants.SHIPMENT)
                .entityPayload(Base64.getEncoder().encodeToString(JsonCompression.compressJson("ShipmentPayload"))).build();
        when(logsHistoryRepository.findByEntityGuidAndTimeStamp(entityGuid, timeStamp)).thenReturn(Optional.of(logsHistory));
        var response = logsHistoryDao.findByEntityGuidAndTimeStamp(entityGuid, timeStamp);
        assertTrue(response.isPresent());
        assertEquals(logsHistory, response.get());
    }

    @Test
    void testFindByEntityGuidsAndTimeStamp() throws IOException {
        UUID entityGuid = UUID.randomUUID();
        LocalDateTime timeStamp = LocalDateTime.now();
        LogsHistory logsHistory = LogsHistory.builder().entityId(1L).entityGuid(entityGuid).entityType(Constants.SHIPMENT)
                .entityPayload(Base64.getEncoder().encodeToString(JsonCompression.compressJson("ShipmentPayload"))).build();
        when(logsHistoryRepository.findByEntityGuidsAndTimeStamp(List.of(entityGuid), timeStamp)).thenReturn(List.of(logsHistory));
        var response = logsHistoryDao.findByEntityGuidsAndTimeStamp(List.of(entityGuid), timeStamp);
        assertFalse(response.isEmpty());
        assertEquals(logsHistory, response.get(0));
    }
}
