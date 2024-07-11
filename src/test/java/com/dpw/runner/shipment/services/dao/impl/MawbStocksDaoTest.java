package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.testcontainers.containers.PostgreSQLContainer;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@RunWith(SpringRunner.class)
@ExtendWith(MockitoExtension.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class MawbStocksDaoTest {

    private static JsonTestUtility jsonTestUtility;
    private static MawbStocks testData;

    @Autowired
    private IMawbStocksDao dao;
    private static final PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");


    static {
        postgresContainer.withDatabaseName("integration-tests-db")
                .withUsername("sa")
                .withPassword("sa").withReuse(true);
        postgresContainer.start();
    }

    @BeforeAll
    static void beforeAll() throws IOException {
        postgresContainer.start();
        jsonTestUtility = new JsonTestUtility();
    }

    @DynamicPropertySource
    static void dynamicConfiguration(DynamicPropertyRegistry registry){
        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
        registry.add("spring.datasource.username", postgresContainer::getUsername);
        registry.add("spring.datasource.password", postgresContainer::getPassword);
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

    @AfterAll
    static void afterAll() {
        postgresContainer.stop();
    }


    @Test
    void save() {
        var result = dao.save(testData);
        assertNotNull(result);
        assertNotNull(result.getId());
    }

    @Test
    void findAll() {
        testData.setStatus("TEST_STATUS");
        dao.save(testData);
        Specification<MawbStocks> spec = (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("status"), "TEST_STATUS");
        };
        var result = dao.findAll(spec, PageRequest.of(0 , 10));
        assertFalse(result.isEmpty());
        assertEquals("TEST_STATUS", result.stream().toList().get(0).getStatus());
    }

    @Test
    void findById() {
        var savedMawbStock = dao.save(testData);
        var result = dao.findById(savedMawbStock.getId());
        assertNotNull(result);
        assertEquals(savedMawbStock.getId(), result.get().getId());
    }

    @Test
    void delete() {
        testData.setMawbStocksLinkRows(null);
        var savedMawbStock = dao.save(testData);
        dao.delete(savedMawbStock);
        var result = dao.findById(savedMawbStock.getId());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void findByGuid() {
        var savedStockData = dao.save(testData);
        var result = dao.findByGuid(savedStockData.getGuid());
        assertNotNull(result);
        assertTrue(result.isPresent());
    }
}