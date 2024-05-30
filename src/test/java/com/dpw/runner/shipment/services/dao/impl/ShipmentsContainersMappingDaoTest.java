package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.ShipmentsContainersMapping;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentsContainersMappingRepository;
import com.dpw.runner.shipment.services.syncing.interfaces.IContainersSync;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
@ExtendWith(MockitoExtension.class)
class ShipmentsContainersMappingDaoTest {

    @Mock
    private IShipmentsContainersMappingRepository shipmentsContainersMappingRepository;

    @Mock
    IContainersSync containersSync;

    @InjectMocks
    private ShipmentsContainersMappingDao shipmentsContainersMappingDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    private ShipmentsContainersMapping testShipmentsContainersMapping;
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
        testShipmentsContainersMapping = jsonTestUtility.getTestShipmentsContainersMapping();
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
    void testFindByContainerId_Success() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        when(shipmentsContainersMappingRepository.findByContainerId(shipmentsContainersMapping.getContainerId())).thenReturn(List.of(shipmentsContainersMapping));
        List<ShipmentsContainersMapping> response = shipmentsContainersMappingDao.findByContainerId(shipmentsContainersMapping.getContainerId());
        assertEquals(List.of(shipmentsContainersMapping), response);
    }

    @Test
    void testFindByShipmentId_Success() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        when(shipmentsContainersMappingRepository.findByShipmentId(shipmentsContainersMapping.getShipmentId())).thenReturn(List.of(shipmentsContainersMapping));
        List<ShipmentsContainersMapping> response = shipmentsContainersMappingDao.findByShipmentId(shipmentsContainersMapping.getShipmentId());
        assertEquals(List.of(shipmentsContainersMapping), response);
    }

    @Test
    void testFindAll_Success() {
        Specification<ShipmentsContainersMapping> specification = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(shipmentsContainersMappingRepository.findAll(specification, pageable)).thenReturn(new PageImpl<>(List.of(testShipmentsContainersMapping)));
        Page<ShipmentsContainersMapping> response = shipmentsContainersMappingDao.findAll(specification, pageable);
        assertEquals(new PageImpl<>(List.of(testShipmentsContainersMapping)), response);
    }

    @Test
    void testFindAllByContainerIds_Success() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(new PageImpl<>(List.of(testShipmentsContainersMapping))).when(spyService).findAll(any(), any());
        Page<ShipmentsContainersMapping> response = spyService.findAllByContainerIds(List.of(shipmentsContainersMapping.getContainerId()));
        assertEquals(new PageImpl<>(List.of(testShipmentsContainersMapping)), response);
    }

    @Test
    void testFindAllByContainerIds_Success_NullContainerIds() {
        Page<ShipmentsContainersMapping> response = shipmentsContainersMappingDao.findAllByContainerIds(List.of());
        assertNull(response);
    }

    @Test
    void testAssignContainers_Success() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        ShipmentsContainersMapping shipmentsContainersMapping1 = new ShipmentsContainersMapping();
        shipmentsContainersMapping1.setContainerId(14L);
        shipmentsContainersMapping1.setShipmentId(31L);

        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(List.of(shipmentsContainersMapping)).when(spyService).findByShipmentId(shipmentsContainersMapping.getShipmentId());
        when(shipmentsContainersMappingRepository.save(any(ShipmentsContainersMapping.class))).thenReturn(shipmentsContainersMapping1);
        assertDoesNotThrow(() ->spyService.assignContainers(shipmentsContainersMapping.getShipmentId(), List.of(shipmentsContainersMapping.getContainerId(), shipmentsContainersMapping1.getContainerId())));
        verify(shipmentsContainersMappingRepository, times(1)).save(any(ShipmentsContainersMapping.class));
    }

    @Test
    void testAssignContainers_Success1() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        ShipmentsContainersMapping shipmentsContainersMapping1 = new ShipmentsContainersMapping();
        shipmentsContainersMapping1.setContainerId(14L);
        shipmentsContainersMapping1.setShipmentId(31L);

        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(List.of(shipmentsContainersMapping)).when(spyService).findByShipmentId(shipmentsContainersMapping.getShipmentId());
        when(shipmentsContainersMappingRepository.save(any(ShipmentsContainersMapping.class))).thenReturn(shipmentsContainersMapping1);
        when(containersSync.sync(any(), any())).thenThrow(new RuntimeException());
        assertDoesNotThrow(() ->spyService.assignContainers(shipmentsContainersMapping.getShipmentId(), List.of(shipmentsContainersMapping.getContainerId(), shipmentsContainersMapping1.getContainerId())));
        verify(shipmentsContainersMappingRepository, times(1)).save(any(ShipmentsContainersMapping.class));
    }

    @Test
    void testAssignShipments_Success() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        ShipmentsContainersMapping shipmentsContainersMapping1 = new ShipmentsContainersMapping();
        shipmentsContainersMapping1.setContainerId(12L);
        shipmentsContainersMapping1.setShipmentId(32L);

        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(List.of(shipmentsContainersMapping)).when(spyService).findByContainerId(shipmentsContainersMapping.getContainerId());
        when(shipmentsContainersMappingRepository.save(any(ShipmentsContainersMapping.class))).thenReturn(shipmentsContainersMapping1);
        assertDoesNotThrow(() ->spyService.assignShipments(shipmentsContainersMapping.getContainerId(), List.of(shipmentsContainersMapping.getShipmentId(), shipmentsContainersMapping1.getShipmentId()), false));
        verify(shipmentsContainersMappingRepository, times(1)).save(any(ShipmentsContainersMapping.class));
    }

    @Test
    void testAssignShipments_Success1() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        ShipmentsContainersMapping shipmentsContainersMapping1 = new ShipmentsContainersMapping();
        shipmentsContainersMapping1.setContainerId(12L);
        shipmentsContainersMapping1.setShipmentId(32L);

        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(List.of(shipmentsContainersMapping)).when(spyService).findByContainerId(shipmentsContainersMapping.getContainerId());
        when(shipmentsContainersMappingRepository.save(any(ShipmentsContainersMapping.class))).thenReturn(shipmentsContainersMapping1);
        when(containersSync.sync(any(), any())).thenThrow(new RuntimeException());
        assertDoesNotThrow(() ->spyService.assignShipments(shipmentsContainersMapping.getContainerId(), List.of(shipmentsContainersMapping.getShipmentId(), shipmentsContainersMapping1.getShipmentId()), false));
        verify(shipmentsContainersMappingRepository, times(1)).save(any(ShipmentsContainersMapping.class));
    }

    @Test
    void testDetachShipments_Success() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        ShipmentsContainersMapping shipmentsContainersMapping1 = new ShipmentsContainersMapping();
        shipmentsContainersMapping1.setContainerId(12L);
        shipmentsContainersMapping1.setShipmentId(32L);

        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(List.of(shipmentsContainersMapping, shipmentsContainersMapping1)).when(spyService).findByContainerId(shipmentsContainersMapping.getContainerId());
        assertDoesNotThrow(() ->spyService.detachShipments(shipmentsContainersMapping.getContainerId(), List.of(shipmentsContainersMapping.getShipmentId()), false));
        verify(shipmentsContainersMappingRepository, times(1)).delete(any(ShipmentsContainersMapping.class));
    }

    @Test
    void testDetachShipments_Success1() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        ShipmentsContainersMapping shipmentsContainersMapping1 = new ShipmentsContainersMapping();
        shipmentsContainersMapping1.setContainerId(12L);
        shipmentsContainersMapping1.setShipmentId(32L);

        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(List.of(shipmentsContainersMapping, shipmentsContainersMapping1)).when(spyService).findByContainerId(shipmentsContainersMapping.getContainerId());
        when(containersSync.sync(any(), any())).thenThrow(new RuntimeException());
        assertDoesNotThrow(() ->spyService.detachShipments(shipmentsContainersMapping.getContainerId(), List.of(shipmentsContainersMapping.getShipmentId()), false));
        verify(shipmentsContainersMappingRepository, times(1)).delete(any(ShipmentsContainersMapping.class));
    }

    @Test
    void testUpdateShipmentsMappings_Success() {
        ShipmentsContainersMapping shipmentsContainersMapping = testShipmentsContainersMapping;
        ShipmentsContainersMapping shipmentsContainersMapping1 = new ShipmentsContainersMapping();
        shipmentsContainersMapping1.setContainerId(12L);
        shipmentsContainersMapping1.setShipmentId(32L);
        ShipmentsContainersMapping shipmentsContainersMapping2 = new ShipmentsContainersMapping();
        shipmentsContainersMapping2.setContainerId(12L);
        shipmentsContainersMapping2.setShipmentId(33L);

        var spyService = Mockito.spy(shipmentsContainersMappingDao);
        doReturn(List.of(shipmentsContainersMapping, shipmentsContainersMapping1)).when(spyService).findByContainerId(shipmentsContainersMapping.getContainerId());
        when(shipmentsContainersMappingRepository.save(any(ShipmentsContainersMapping.class))).thenReturn(shipmentsContainersMapping2);
        assertDoesNotThrow(() ->spyService.updateShipmentsMappings(shipmentsContainersMapping.getContainerId(), List.of(shipmentsContainersMapping1.getShipmentId(), shipmentsContainersMapping2.getShipmentId())));
        verify(shipmentsContainersMappingRepository, times(1)).delete(any(ShipmentsContainersMapping.class));
        verify(shipmentsContainersMappingRepository, times(1)).save(any(ShipmentsContainersMapping.class));
    }

}
