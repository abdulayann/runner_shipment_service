package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentSettingsRepository;
import com.dpw.runner.shipment.services.service.impl.CacheEvictionService;
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
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentSettingsDaoTest {

    @Mock
    private IShipmentSettingsRepository shipmentSettingsRepository;

    @Mock
    private CacheEvictionService cacheEvictionService;
    @InjectMocks
    private ShipmentSettingsDao shipmentSettingsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    private static ShipmentSettingsDetails testShipmentSettingsDetails;

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
        testShipmentSettingsDetails = jsonTestUtility.getTestShipmentSettingsDetails();
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
    void testSave() {
        when(shipmentSettingsRepository.save(any(ShipmentSettingsDetails.class))).thenReturn(ShipmentSettingsDaoTest.testShipmentSettingsDetails);

        ShipmentSettingsDetails savedShipmentSettingsDetails = shipmentSettingsDao.save(ShipmentSettingsDaoTest.testShipmentSettingsDetails);

        assertEquals(ShipmentSettingsDaoTest.testShipmentSettingsDetails, savedShipmentSettingsDetails);
    }

    @Test
    void testSave_Branch() {
        ShipmentSettingsDetails shipmentSettingsDetails = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingsDetails.class);
        shipmentSettingsDetails.setCancelledBLSuffix("1234");
        when(shipmentSettingsRepository.save(any(ShipmentSettingsDetails.class))).thenReturn(ShipmentSettingsDaoTest.testShipmentSettingsDetails);

        ShipmentSettingsDetails savedShipmentSettingsDetails = shipmentSettingsDao.save(shipmentSettingsDetails);

        assertEquals(ShipmentSettingsDaoTest.testShipmentSettingsDetails, savedShipmentSettingsDetails);
    }

    @Test
    void testSave_Failure() {
        ShipmentSettingsDetails shipmentSettingsDetails = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingsDetails.class);
        shipmentSettingsDetails.setCancelledBLSuffix("123456");
        assertThrows(ValidationException.class, () -> shipmentSettingsDao.save(shipmentSettingsDetails));
    }

    @Test
    void testFindAll() {
        Page<ShipmentSettingsDetails> testShipmentSettingsDetailsPage = mock(Page.class);
        Specification<ShipmentSettingsDetails> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(shipmentSettingsRepository.findAll(spec, pageable)).thenReturn(testShipmentSettingsDetailsPage);

        Page<ShipmentSettingsDetails> foundShipmentSettingsDetailsPage = shipmentSettingsDao.list(spec, pageable);

        assertEquals(testShipmentSettingsDetailsPage, foundShipmentSettingsDetailsPage);
    }

    @Test
    void testFindById() {
        Optional<ShipmentSettingsDetails> optionalShipmentSettingsDetails = Optional.of(ShipmentSettingsDaoTest.testShipmentSettingsDetails);
        when(shipmentSettingsRepository.findById(anyLong())).thenReturn(optionalShipmentSettingsDetails);

        Optional<ShipmentSettingsDetails> foundShipmentSettingsDetails = shipmentSettingsDao.findById(1L);

        assertTrue(foundShipmentSettingsDetails.isPresent());
        assertEquals(ShipmentSettingsDaoTest.testShipmentSettingsDetails, foundShipmentSettingsDetails.get());
    }
    @Test
    void testFindByGuid() {
        Optional<ShipmentSettingsDetails> optionalShipmentSettingsDetails = Optional.of(ShipmentSettingsDaoTest.testShipmentSettingsDetails);
        when(shipmentSettingsRepository.findByGuid(any(UUID.class))).thenReturn(optionalShipmentSettingsDetails);

        Optional<ShipmentSettingsDetails> foundShipmentSettingsDetails = shipmentSettingsDao.findByGuid(UUID.randomUUID());

        assertTrue(foundShipmentSettingsDetails.isPresent());
        assertEquals(ShipmentSettingsDaoTest.testShipmentSettingsDetails, foundShipmentSettingsDetails.get());
    }

    @Test
    void testDelete() {
        assertDoesNotThrow(() -> shipmentSettingsDao.delete(ShipmentSettingsDaoTest.testShipmentSettingsDetails));
        verify(shipmentSettingsRepository, Mockito.times(1)).delete(ShipmentSettingsDaoTest.testShipmentSettingsDetails);
    }

    @Test
    void testList() {
        when(shipmentSettingsRepository.findAll()).thenReturn(List.of(testShipmentSettingsDetails));
        List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.list();
        assertEquals(List.of(testShipmentSettingsDetails), shipmentSettingsDetails);
    }

    @Test
    void testGetSettingsByTenantIds() {
        when(shipmentSettingsRepository.getTenantSetting(any())).thenReturn(List.of(testShipmentSettingsDetails));
        List<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIds(List.of(1));
        assertEquals(List.of(testShipmentSettingsDetails), shipmentSettingsDetails);
    }

    @Test
    void testFindByTenantId() {
        when(shipmentSettingsRepository.findByTenantId(any())).thenReturn(Optional.of(testShipmentSettingsDetails));
        Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.findByTenantId(1);
        assertEquals(Optional.of(testShipmentSettingsDetails), shipmentSettingsDetails);
    }

    @Test
    void testGetShipmentConsoleImportApprovarRole() {
        when(shipmentSettingsRepository.getShipmentConsoleImportApprovarRole(anyInt())).thenReturn(1);
        Integer shipmentConsoleImportApprovarRole = shipmentSettingsDao.getShipmentConsoleImportApprovarRole(1);
        assertEquals(1, shipmentConsoleImportApprovarRole);
    }

    @Test
    void getSettingsByTenantIdWithCache() {
        when(shipmentSettingsRepository.findByTenantId(any())).thenReturn(Optional.of(testShipmentSettingsDetails));
        Optional<ShipmentSettingsDetails> shipmentSettingsDetails = shipmentSettingsDao.getSettingsByTenantIdWithCache(1);
        assertEquals(Optional.of(testShipmentSettingsDetails), shipmentSettingsDetails);
    }

}
