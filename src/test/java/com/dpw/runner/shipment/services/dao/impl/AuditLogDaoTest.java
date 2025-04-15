package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.entity.AuditLog;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IAuditLogRepository;
import java.time.LocalDateTime;
import javax.persistence.EntityManager;
import javax.persistence.Query;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AuditLogDaoTest {

    @Mock
    private IAuditLogRepository auditLogRepository;

    @InjectMocks
    private AuditLogDao auditLogDao;

    @Mock
    private EntityManager entityManager;

    @Mock
    private JsonHelper jsonHelper;

    private AuditLog auditLog;

    @BeforeEach
    void setUp() {
        auditLog = new AuditLog();
        auditLog.setId(1L);
    }

    @Test
    void testSave() {
        Query queryMock = mock(Query.class);
        when(entityManager.createNativeQuery(anyString())).thenReturn(queryMock);
        when(queryMock.setParameter(anyInt(), any())).thenReturn(queryMock);

        auditLog.setUpdatedAt(LocalDateTime.now());
        auditLog.setCreatedAt(LocalDateTime.now());
        auditLogDao.save(auditLog);

        verify(entityManager, times(1)).createNativeQuery(anyString());
    }

    @Test
    void testSaveAll() {
        List<AuditLog> auditLogs = Arrays.asList(auditLog);
        when(auditLogRepository.saveAll(auditLogs)).thenReturn(auditLogs);
        List<AuditLog> savedAuditLogs = auditLogDao.saveAll(auditLogs);
        assertEquals(auditLogs, savedAuditLogs);
        verify(auditLogRepository, times(1)).saveAll(auditLogs);
    }

    @Test
    void testFindAll() {
        Pageable pageable = PageRequest.of(0, 10);
        Page<AuditLog> auditLogPage = new PageImpl<>(Arrays.asList(auditLog));
        Specification<AuditLog> spec = mock(Specification.class);

        when(auditLogRepository.findAll(spec, pageable)).thenReturn(auditLogPage);
        Page<AuditLog> foundAuditLogs = auditLogDao.findAll(spec, pageable);
        assertEquals(auditLogPage, foundAuditLogs);
        verify(auditLogRepository, times(1)).findAll(spec, pageable);
    }

    @Test
    void testFindById() {
        when(auditLogRepository.findById(1L)).thenReturn(Optional.of(auditLog));
        Optional<AuditLog> foundAuditLog = auditLogDao.findById(1L);
        assertEquals(Optional.of(auditLog), foundAuditLog);
        verify(auditLogRepository, times(1)).findById(1L);
    }

    @Test
    void testDelete() {
        doNothing().when(auditLogRepository).delete(auditLog);
        auditLogDao.delete(auditLog);
        verify(auditLogRepository, times(1)).delete(auditLog);
    }
}