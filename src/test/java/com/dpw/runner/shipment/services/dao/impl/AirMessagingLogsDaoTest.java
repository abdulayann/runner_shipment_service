package com.dpw.runner.shipment.services.dao.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AirMessagingLogs;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IAirMessagingLogsRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class AirMessagingLogsDaoTest {
    @Mock
    private IAirMessagingLogsRepository airMessagingLogsRepository;
    @InjectMocks
    private AirMessagingLogsDao airMessagingLogsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private AirMessagingLogs testairMessagingLogs;
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
        testairMessagingLogs = jsonTestUtility.getTestAirMessagingLogs();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void testSave_Success() {
        AirMessagingLogs airMessagingLogs = testairMessagingLogs;
        when(airMessagingLogsRepository.save(airMessagingLogs)).thenReturn(airMessagingLogs);
        AirMessagingLogs response = airMessagingLogsDao.save(airMessagingLogs);
        assertEquals(airMessagingLogs, response);
    }

    @Test
    void testFindAll_Success() {
        Page<AirMessagingLogs> airMessagingLogsPage = mock(Page.class);
        Specification<AirMessagingLogs> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(airMessagingLogsRepository.findAll(spec, pageable)).thenReturn(airMessagingLogsPage);
        Page<AirMessagingLogs> airMessagingLogs = airMessagingLogsDao.findAll(spec, pageable);
        assertEquals(airMessagingLogsPage, airMessagingLogs);
    }

    @Test
    void testFindById_Success() {
        AirMessagingLogs airMessagingLogs = testairMessagingLogs;
        when(airMessagingLogsRepository.findById(airMessagingLogs.getId())).thenReturn(Optional.of(airMessagingLogs));
        Optional<AirMessagingLogs> response = airMessagingLogsDao.findById(airMessagingLogs.getId());
        assertTrue(response.isPresent());
        assertEquals(airMessagingLogs, response.get());
    }

    @Test
    void testDelete_Success() {
        AirMessagingLogs airMessagingLogs = testairMessagingLogs;
        airMessagingLogsDao.delete(airMessagingLogs);
        verify(airMessagingLogsRepository, times(1)).delete(airMessagingLogs);
    }

    @Test
    void testFindByEntityGuid_Success() {
        AirMessagingLogs airMessagingLogs = testairMessagingLogs;
        when(airMessagingLogsRepository.findByEntityGuid(airMessagingLogs.getGuid())).thenReturn(List.of(airMessagingLogs));
        List<AirMessagingLogs> response = airMessagingLogsDao.findByEntityGuid(airMessagingLogs.getGuid());
        assertEquals(List.of(airMessagingLogs), response);
    }

    @Test
    void testFindByEntityGuidByQuery_Success() {
        AirMessagingLogs airMessagingLogs = testairMessagingLogs;
        when(airMessagingLogsRepository.findByEntityGuidByQuery(airMessagingLogs.getGuid())).thenReturn(List.of(airMessagingLogs));
        List<AirMessagingLogs> response = airMessagingLogsDao.findByEntityGuidByQuery(airMessagingLogs.getGuid());
        assertEquals(List.of(airMessagingLogs), response);
    }

    @Test
    void testCreateAirMessagingLogs_Success() {
        AirMessagingLogs airMessagingLogs = testairMessagingLogs;
        airMessagingLogsDao.createAirMessagingLogs(airMessagingLogs.getGuid(), airMessagingLogs.getEntityGuid(), airMessagingLogs.getErrorMessage(), airMessagingLogs.getMessageType(), airMessagingLogs.getXmlPayload(), airMessagingLogs.getStatus(), airMessagingLogs.getTenantId(), airMessagingLogs.getCreatedAt());
        verify(airMessagingLogsRepository, times(1)).createAirMessagingLogs(airMessagingLogs.getGuid(), airMessagingLogs.getEntityGuid(), airMessagingLogs.getErrorMessage(), airMessagingLogs.getMessageType(), airMessagingLogs.getXmlPayload(), airMessagingLogs.getStatus(), airMessagingLogs.getTenantId(), airMessagingLogs.getCreatedAt());
    }
}
