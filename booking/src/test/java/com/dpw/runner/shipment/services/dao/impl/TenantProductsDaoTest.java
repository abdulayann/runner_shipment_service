package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.ITenantProductsRepository;
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
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TenantProductsDaoTest {
    @Mock
    private ITenantProductsRepository tenantProductsRepository;
    @InjectMocks
    private TenantProductsDao tenantProductsDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    private TenantProducts testTenantProducts;

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
        testTenantProducts = jsonTestUtility.getTenantProducts();
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
        TenantProducts tenantProducts = testTenantProducts;
        when(tenantProductsRepository.save(tenantProducts)).thenReturn(tenantProducts);
        TenantProducts response = tenantProductsDao.save(tenantProducts);
        assertEquals(tenantProducts, response);
    }

    @Test
    void testFindById_Success() {
        TenantProducts tenantProducts = testTenantProducts;
        when(tenantProductsRepository.findById(tenantProducts.getId())).thenReturn(Optional.of(tenantProducts));
        Optional<TenantProducts> response = tenantProductsDao.findById(tenantProducts.getId());
        assertTrue(response.isPresent());
        assertEquals(tenantProducts, response.get());
    }

    @Test
    void testSaveAll_Success() {
        TenantProducts tenantProducts = testTenantProducts;
        when(tenantProductsRepository.saveAll(List.of(tenantProducts))).thenReturn(List.of(tenantProducts));
        List<TenantProducts> responseList = tenantProductsDao.saveAll(List.of(tenantProducts));
        assertEquals(List.of(tenantProducts), responseList);
    }

    @Test
    void testFindAll_Success() {
        TenantProducts tenantProducts = testTenantProducts;
        Specification<TenantProducts> specification = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(tenantProductsRepository.findAll(specification, pageable)).thenReturn(new PageImpl<>(List.of(tenantProducts)));
        Page<TenantProducts> response = tenantProductsDao.findAll(specification, pageable);
        assertEquals(new PageImpl<>(List.of(tenantProducts)), response);
    }

    @Test
    void testSaveEntityFromSettings_Success() {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        var spyService = Mockito.spy(tenantProductsDao);
        doReturn(Optional.of(testTenantProducts)).when(spyService).findById(testTenantProducts.getId());
        when(tenantProductsRepository.save(any(TenantProducts.class))).thenReturn(testTenantProducts);
        List<TenantProducts> responseList = spyService.saveEntityFromSettings(tenantProductsList, 1L);
        assertEquals(tenantProductsList, responseList);
    }

    @Test
    void testSaveEntityFromSettings_Failure() {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        var spyService = Mockito.spy(tenantProductsDao);
        doReturn(Optional.empty()).when(spyService).findById(testTenantProducts.getId());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromSettings(tenantProductsList, 1L));
    }

    @Test
    void testUpdateEntityFromSettings_Success() throws RunnerException {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        TenantProducts tenantProducts = jsonTestUtility.getTenantProducts();
        tenantProducts.setId(2L);
        var spyService = Mockito.spy(tenantProductsDao);
        doReturn(new PageImpl<>(List.of(testTenantProducts, tenantProducts))).when(spyService).findAll(any(), any());
        doReturn(tenantProductsList).when(spyService).saveEntityFromSettings(anyList(), anyLong());
        List<TenantProducts> responseList = spyService.updateEntityFromSettings(tenantProductsList, 1L);
        assertEquals(tenantProductsList, responseList);
    }

    @Test
    void testUpdateEntityFromSettings_Failure() throws RunnerException {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        var spyService = Mockito.spy(tenantProductsDao);
        doThrow(new RuntimeException()).when(spyService).findAll(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromSettings(tenantProductsList, 1L));
    }

    @Test
    void testUpdateEntityFromSettings_Failure_Delete() throws RunnerException {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        TenantProducts tenantProducts = jsonTestUtility.getTenantProducts();
        tenantProducts.setId(2L);
        var spyService = Mockito.spy(tenantProductsDao);
        doReturn(new PageImpl<>(List.of(testTenantProducts, tenantProducts))).when(spyService).findAll(any(), any());
        doReturn(tenantProductsList).when(spyService).saveEntityFromSettings(anyList(), anyLong());
        doThrow(new RuntimeException()).when(spyService).delete(any());
        List<TenantProducts> responseList = spyService.updateEntityFromSettings(tenantProductsList, 1L);
        assertEquals(tenantProductsList, responseList);
    }

    @Test
    void testUpdateEntityFromV1Settings_Success() throws RunnerException {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        TenantProducts tenantProducts = jsonTestUtility.getTenantProducts();
        tenantProducts.setGuid(UUID.randomUUID());
        tenantProducts.setId(2L);
        var spyService = Mockito.spy(tenantProductsDao);
        doReturn(tenantProductsList).when(spyService).saveEntityFromSettings(anyList(), anyLong());
        List<TenantProducts> responseList = spyService.updateEntityFromV1Settings(tenantProductsList, 1L, List.of(testTenantProducts, tenantProducts));
        assertEquals(tenantProductsList, responseList);
    }

    @Test
    void testUpdateEntityFromV1Settings_Failure() {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        TenantProducts tenantProducts = jsonTestUtility.getTenantProducts();
        tenantProducts.setGuid(UUID.randomUUID());
        tenantProducts.setId(2L);
        var spyService = Mockito.spy(tenantProductsDao);
        doThrow(new RuntimeException()).when(spyService).saveEntityFromSettings(anyList(), anyLong());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromV1Settings(tenantProductsList, 1L, List.of(testTenantProducts, tenantProducts)));
    }

    @Test
    void testUpdateEntityFromV1Settings_Failure_Delete() throws RunnerException {
        List<TenantProducts> tenantProductsList = List.of(testTenantProducts);
        TenantProducts tenantProducts = jsonTestUtility.getTenantProducts();
        tenantProducts.setGuid(UUID.randomUUID());
        tenantProducts.setId(2L);
        var spyService = Mockito.spy(tenantProductsDao);
        doReturn(tenantProductsList).when(spyService).saveEntityFromSettings(anyList(), anyLong());
        doThrow(new RuntimeException()).when(spyService).delete(any());
        List<TenantProducts> responseList = spyService.updateEntityFromV1Settings(tenantProductsList, 1L, List.of(testTenantProducts, tenantProducts));
        assertEquals(tenantProductsList, responseList);
    }
}
