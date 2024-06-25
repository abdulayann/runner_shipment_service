package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IProductSequenceConfigRepository;
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

import javax.persistence.EntityManager;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ProductSequenceConfigDaoTest {

    @Mock
    private IProductSequenceConfigRepository productSequenceConfigRepository;
    @Mock
    private EntityManager entityManager;

    @InjectMocks
    private ProductSequenceConfigDao productSequenceConfigDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static ProductSequenceConfig testProductSequenceConfig;

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
        testProductSequenceConfig = jsonTestUtility.getTestShipmentSettingsDetails().getProductSequenceConfig().get(0);
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
    void save() {
        Mockito.when(productSequenceConfigRepository.save(any())).thenReturn(testProductSequenceConfig);
        ProductSequenceConfig productSequenceConfig = productSequenceConfigDao.save(testProductSequenceConfig);
        assertEquals(testProductSequenceConfig, productSequenceConfig);
    }

    @Test
    void findById() {
        Mockito.when(productSequenceConfigRepository.findById(any())).thenReturn(Optional.of(testProductSequenceConfig));
        Optional<ProductSequenceConfig> productSequenceConfig = productSequenceConfigDao.findById(1L);
        assertEquals(testProductSequenceConfig, productSequenceConfig.get());
    }

    @Test
    void saveAll() {
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.saveAll(any())).thenReturn(productSequenceConfigList);
        List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.saveAll(productSequenceConfigList);
        assertEquals(productSequenceConfigList, productSequenceConfigs);
    }

    @Test
    void saveEntityFromSettings() {
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findById(any())).thenReturn(Optional.of(testProductSequenceConfig));
        Mockito.when(productSequenceConfigRepository.saveAll(any())).thenReturn(List.of(testProductSequenceConfig));
        List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.saveEntityFromSettings(productSequenceConfigList, 3L);
        assertEquals(productSequenceConfigList, productSequenceConfigs);
    }

    @Test
    void saveEntityFromSettings_IdNull() {
        testProductSequenceConfig.setId(null);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.saveAll(any())).thenReturn(List.of(testProductSequenceConfig));
        List<ProductSequenceConfig> productSequenceConfigs = productSequenceConfigDao.saveEntityFromSettings(productSequenceConfigList, 3L);
        assertEquals(productSequenceConfigList, productSequenceConfigs);
    }

    @Test
    void saveEntityFromSettings_DataRetError() {
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findById(any())).thenReturn(Optional.empty());
        assertThrows(DataRetrievalFailureException.class, () -> productSequenceConfigDao.saveEntityFromSettings(productSequenceConfigList, 3L));
    }

    @Test
    void findAll() {
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl(productSequenceConfigList));
        Specification<ProductSequenceConfig> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        Page<ProductSequenceConfig> productSequenceConfig = productSequenceConfigDao.findAll(spec, pageable);
        assertEquals(productSequenceConfigList, productSequenceConfig.getContent());
    }

    @Test
    void updateEntityFromSettings() throws RunnerException {
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl(productSequenceConfigList));
        doReturn(productSequenceConfigList).when(spyService).saveEntityFromSettings(any(), any());
        List<ProductSequenceConfig> productSequenceConfigs = spyService.updateEntityFromSettings(productSequenceConfigList, 3L);
        assertEquals(productSequenceConfigList, productSequenceConfigs);
    }

    @Test
    void updateEntityFromSettings_ProdSeqNull() throws RunnerException {
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl(productSequenceConfigList));
        doThrow(new RuntimeException()).when(productSequenceConfigRepository).delete(any());
        List<ProductSequenceConfig> productSequenceConfigs = spyService.updateEntityFromSettings(null, 3L);
        assertEquals(new ArrayList<>(), productSequenceConfigs);
    }

    @Test
    void updateEntityFromSettings_ProdSeqEmpty() throws RunnerException {
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl(productSequenceConfigList));
        doThrow(new RuntimeException()).when(productSequenceConfigRepository).delete(any());
        List<ProductSequenceConfig> productSequenceConfigs = spyService.updateEntityFromSettings(new ArrayList<>(), 3L);
        assertEquals(new ArrayList<>(), productSequenceConfigs);
    }

    @Test
    void updateEntityFromSettings_Error() throws RunnerException {
        testProductSequenceConfig.setId(null);
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl(productSequenceConfigList));
        doThrow(new RuntimeException()).when(spyService).saveEntityFromSettings(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromSettings(productSequenceConfigList, 3L));
    }

    @Test
    void updateEntityFromV1Settings() throws RunnerException {
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        doReturn(productSequenceConfigList).when(spyService).saveEntityFromSettings(any(), any());
        List<ProductSequenceConfig> productSequenceConfigs = spyService.updateEntityFromV1Settings(productSequenceConfigList, 3L, productSequenceConfigList);
        assertEquals(productSequenceConfigList, productSequenceConfigs);
    }

    @Test
    void updateEntityFromV1Settings_ProdSeqNull() throws RunnerException {
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        doThrow(new RuntimeException()).when(productSequenceConfigRepository).delete(any());
        List<ProductSequenceConfig> productSequenceConfigs = spyService.updateEntityFromV1Settings(null, 3L, productSequenceConfigList);
        assertEquals(new ArrayList<>(), productSequenceConfigs);
    }

    @Test
    void updateEntityFromV1Settings_ProdSeqEmpty() throws RunnerException {
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        doThrow(new RuntimeException()).when(productSequenceConfigRepository).delete(any());
        List<ProductSequenceConfig> productSequenceConfigs = spyService.updateEntityFromV1Settings(new ArrayList<>(), 3L, productSequenceConfigList);
        assertEquals(new ArrayList<>(), productSequenceConfigs);
    }

    @Test
    void updateEntityFromV1Settings_Error() throws RunnerException {
        testProductSequenceConfig.setId(null);
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        doThrow(new RuntimeException()).when(spyService).saveEntityFromSettings(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromV1Settings(productSequenceConfigList, 3L, null));
    }

    @Test
    void updateEntityFromV1Settings_Error_EmptyOldProdSeq() throws RunnerException {
        testProductSequenceConfig.setId(null);
        ProductSequenceConfigDao spyService = spy(productSequenceConfigDao);
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        doThrow(new RuntimeException()).when(spyService).saveEntityFromSettings(any(), any());
        assertThrows(RunnerException.class, () -> spyService.updateEntityFromV1Settings(productSequenceConfigList, 3L, new ArrayList<>()));
    }

    @Test
    void findAndLock() {
        List<ProductSequenceConfig> productSequenceConfigList = List.of(testProductSequenceConfig);
        Mockito.when(productSequenceConfigRepository.findAll(any(Specification.class), any(Pageable.class))).thenReturn(new PageImpl(productSequenceConfigList));
        Specification<ProductSequenceConfig> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        var productSequenceConfig = productSequenceConfigDao.findAndLock(spec, pageable);

        verify(entityManager, times(1)).lock(any(), any(), any());
        assertEquals(testProductSequenceConfig, productSequenceConfig);
    }
}
