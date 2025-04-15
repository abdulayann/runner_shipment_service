package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IConsoleShipmentsMappingRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsoleShipmentMappingDaoTest {

    @Mock
    private IConsoleShipmentsMappingRepository consoleShipmentsMappingRepository;

    @InjectMocks
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static ConsoleShipmentMapping testConsoleShipmentMapping;

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
        testConsoleShipmentMapping = new ConsoleShipmentMapping();
        testConsoleShipmentMapping.setShipmentId(1L);
        testConsoleShipmentMapping.setConsolidationId(2L);
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
    void findAll() {
        List<ConsoleShipmentMapping> consoleShipmentMappings = List.of(testConsoleShipmentMapping);
        Mockito.when(consoleShipmentsMappingRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl(consoleShipmentMappings));
        Specification<ConsoleShipmentMapping> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        Page<ConsoleShipmentMapping> consoleShipmentMapping = consoleShipmentMappingDao.findAll(spec, pageable);
        assertEquals(consoleShipmentMappings, consoleShipmentMapping.getContent());
    }

    @Test
    void testFindByConsolidationId() {
        Long consolidationId = 1L;
        List<ConsoleShipmentMapping> listResponse = List.of(testConsoleShipmentMapping);
        when(consoleShipmentsMappingRepository.findByConsolidationIdByQuery(consolidationId)).thenReturn(listResponse);
        var result = consoleShipmentMappingDao.findByConsolidationId(consolidationId);
        assertEquals(listResponse, result);
    }

    @Test
    void testFindByShipmentId() {
        Long shipmentId = 1L;
        List<ConsoleShipmentMapping> listResponse = List.of(testConsoleShipmentMapping);
        when(consoleShipmentsMappingRepository.findByShipmentIdByQuery(shipmentId)).thenReturn(listResponse);
        var result = consoleShipmentMappingDao.findByShipmentId(shipmentId);
        assertEquals(listResponse, result);
    }

    @Test
    void testFindByShipmentIdQuery() {
        Long shipmentId = 1L;
        List<ConsoleShipmentMapping> listResponse = List.of(testConsoleShipmentMapping);
        when(consoleShipmentsMappingRepository.findByShipmentIdByQuery(shipmentId)).thenReturn(listResponse);
        var result = consoleShipmentMappingDao.findByShipmentIdByQuery(shipmentId);
        assertEquals(listResponse, result);
    }

    @Test
    void testFindByConsolidationIdQuery() {
        Long consolidationId = 1L;
        List<ConsoleShipmentMapping> listResponse = List.of(testConsoleShipmentMapping);
        when(consoleShipmentsMappingRepository.findByConsolidationIdByQuery(consolidationId)).thenReturn(listResponse);
        var result = consoleShipmentMappingDao.findByConsolidationIdByQuery(consolidationId);
        assertEquals(listResponse, result);
    }

    @Test
    void detachShipments() {
        List<Long> shipIds = consoleShipmentMappingDao.detachShipments(1L, List.of(3L));
        assertEquals(List.of(3L), shipIds);
    }

    @Test
    void assignShipments() {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        doReturn(consoleShipmentMappingList).when(consoleShipmentsMappingRepository).findByConsolidationIdByQuery(any());
        List<Long> shipIds = List.of(2L);
        Set<Long> response = consoleShipmentMappingDao.assignShipments(ShipmentRequestedType.APPROVE, 1L, shipIds, null, new HashSet<Long>(), new HashSet<>(new ArrayList<>(shipIds)), new HashMap<>());
        assertEquals(new HashSet<>(shipIds), response);
    }

    @Test
    void assignShipmentsWithInterBranchShipments() {
        Long shipmentId = 2L;
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        doReturn(consoleShipmentMappingList).when(consoleShipmentsMappingRepository).findByConsolidationIdByQuery(any());
        List<Long> shipIds = List.of(shipmentId);
        Set<Long> response = consoleShipmentMappingDao.assignShipments(ShipmentRequestedType.APPROVE, 1L, shipIds, null, new HashSet<Long>(List.of(shipmentId)), new HashSet<>(new ArrayList<>(shipIds)), new HashMap<>());
        assertEquals(new HashSet<>(shipIds), response);
    }

    @Test
    void assignShipmentsWithImportInterBranchShipments() {
        Long shipmentId = 2L;
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        doReturn(consoleShipmentMappingList).when(consoleShipmentsMappingRepository).findByConsolidationIdByQuery(any());
        List<Long> shipIds = List.of(shipmentId);
        Map<Long, ShipmentDetails> interBranchImportShipmentMap = new HashMap<>();
        ShipmentDetails shipmentDetails = mock(ShipmentDetails.class);
        interBranchImportShipmentMap.put(shipmentId, shipmentDetails);
        Set<Long> response = consoleShipmentMappingDao.assignShipments(ShipmentRequestedType.APPROVE, 1L, shipIds, null, new HashSet<Long>(List.of(shipmentId)), new HashSet<>(new ArrayList<>(shipIds)), interBranchImportShipmentMap);
        assertEquals(new HashSet<>(shipIds), response);
    }

    @Test
    void assignShipments_MappingsNull() {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        doReturn(null).when(consoleShipmentsMappingRepository).findByConsolidationIdByQuery(any());
        List<Long> shipIds = List.of(2L);
        Set<Long> response = consoleShipmentMappingDao.assignShipments(ShipmentRequestedType.APPROVE, 1L, shipIds, null, new HashSet<Long>(), new HashSet<>(new ArrayList<>(shipIds)), new HashMap<>());
        assertEquals(new HashSet<>(shipIds), response);
    }

    @Test
    void assignShipments_MappingsEmpty() {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        doReturn(new ArrayList<>()).when(consoleShipmentsMappingRepository).findByConsolidationIdByQuery(any());
        List<Long> shipIds = List.of(2L);
        Set<Long> response = consoleShipmentMappingDao.assignShipments(ShipmentRequestedType.APPROVE, 1L, shipIds, null, new HashSet<Long>(), new HashSet<>(new ArrayList<>(shipIds)), new HashMap<>());
        assertEquals(new HashSet<>(shipIds), response);
    }

    @Test
    void assignShipments_Branches() {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        List<Long> shipIds = new ArrayList<>();
        Set<Long> response = consoleShipmentMappingDao.assignShipments(ShipmentRequestedType.APPROVE, 1L, new ArrayList<>(), consoleShipmentMappingList, new HashSet<Long>(), new HashSet<>(new ArrayList<>(shipIds)), new HashMap<>());
        assertEquals(new HashSet<>(shipIds), response);
    }

    @Test
    void updateShipmentsMappings() {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        doReturn(consoleShipmentMappingList).when(consoleShipmentsMappingRepository).findByConsolidationIdByQuery(any());
        List<Long> shipIds = List.of(2L);
        assertDoesNotThrow(() -> consoleShipmentMappingDao.updateShipmentsMappings(1L, shipIds));
    }

    @Test
    void updateShipmentsMappings_Cases() {
        List<ConsoleShipmentMapping> consoleShipmentMappingList = List.of(testConsoleShipmentMapping);
        doReturn(consoleShipmentMappingList).when(consoleShipmentsMappingRepository).findByConsolidationIdByQuery(any());
        List<Long> shipIds = List.of(1L);
        assertDoesNotThrow(() -> consoleShipmentMappingDao.updateShipmentsMappings(1L, shipIds));
    }

    @Test
    void testFindByConsolidationIdAll() {
        Long consolId = 1L;
        consoleShipmentMappingDao.findByConsolidationIdAll(consolId);
        verify(consoleShipmentsMappingRepository, times(1)).findByConsolidationId(any());
    }

    @Test
    void testDeletePendingStateByConsoleId() {
        boolean isSuccess = true;
        doNothing().when(consoleShipmentsMappingRepository).deletePendingStateByConsoleId(anyLong());
        consoleShipmentMappingDao.deletePendingStateByConsoleId(1L);
        assertTrue(isSuccess);
    }


}
