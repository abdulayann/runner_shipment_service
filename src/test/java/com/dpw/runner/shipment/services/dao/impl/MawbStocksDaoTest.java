package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IMawbStocksRepository;
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
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class MawbStocksDaoTest {

    @Mock
    private IMawbStocksRepository mawbStocksRepository;

    private static JsonTestUtility jsonTestUtility;
    private static MawbStocks testData;

    @InjectMocks
    private MawbStocksDao dao;


    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        testData = jsonTestUtility.getTestStockData();
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }



    @Test
    void save() {
        testData.setId(12L);
        Mockito.when(mawbStocksRepository.save(Mockito.any())).thenReturn(testData);
        var result = dao.save(testData);
        assertNotNull(result);
        assertNotNull(result.getId());
    }

    @Test
    void findAll() {
        testData.setStatus("TEST_STATUS");
        Specification<MawbStocks> spec = (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("status"), "TEST_STATUS");
        };
        Page<MawbStocks> page = new PageImpl<>(List.of(testData));
        Mockito.when(mawbStocksRepository.findAll(Mockito.any(Specification.class), Mockito.any(Pageable.class))).thenReturn(page);
        var result = dao.findAll(spec, PageRequest.of(0 , 10));
        assertFalse(result.isEmpty());
        assertEquals("TEST_STATUS", result.stream().toList().get(0).getStatus());
    }

    @Test
    void findById() {
        testData.setId(12L);
        Mockito.when(mawbStocksRepository.findById(Mockito.any())).thenReturn(Optional.ofNullable(testData));
        var result = dao.findById(testData.getId());
        assertNotNull(result);
        assertEquals(testData.getId(), result.get().getId());
    }

    @Test
    void delete() {
        testData.setMawbStocksLinkRows(null);
        dao.delete(testData);
        Mockito.verify(mawbStocksRepository, Mockito.times(1)).delete(testData);
    }

    @Test
    void findByGuid() {
        testData.setGuid(UUID.randomUUID());
        Mockito.when(mawbStocksRepository.findByGuid(Mockito.any())).thenReturn(Optional.of(testData));
        var result = dao.findByGuid(testData.getGuid());
        assertNotNull(result);
        assertTrue(result.isPresent());
    }
}