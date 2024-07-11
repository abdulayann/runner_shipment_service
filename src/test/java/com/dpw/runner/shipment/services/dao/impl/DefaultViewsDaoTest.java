package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.DefaultViews;
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
class DefaultViewsDaoTest {

    @Autowired
    private DefaultViewsDao dao;
    private static JsonTestUtility jsonTestUtility;
    private static DefaultViews testData;


    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");

    static {
        postgresContainer = (PostgreSQLContainer<?>) new PostgreSQLContainer("postgres:15-alpine")
                .withDatabaseName("integration-tests-db")
                .withUsername("sa")
                .withPassword("sa").withReuse(true);
        postgresContainer.start();
    }

    @BeforeAll
    static void beforeAll() throws IOException {
        postgresContainer.start();
        jsonTestUtility = new JsonTestUtility();
    }

    @AfterAll
    static void afterAll() {
        postgresContainer.stop();
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
        testData = jsonTestUtility.getTestDefaultView();
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        var savedEntity = dao.save(testData);
        assertNotNull(savedEntity);
        assertNotNull(savedEntity.getId());
    }

    @Test
    void findAll() {
        var savedEntity = dao.save(testData);
        assertNotNull(savedEntity.getId());
        var result = dao.findAll();
        assertNotNull(result);
        assertTrue(result.stream().anyMatch(c -> c.getId() == savedEntity.getId()));
    }

    @Test
    void findById() {
        var savedEntity = dao.save(testData);
        assertNotNull(savedEntity.getId());
        var result = dao.findById(savedEntity.getId());
        assertNotNull(result);
        assertEquals(result.get().getId(), savedEntity.getId());
    }

    @Test
    void delete() {
        var savedEntity = dao.save(testData);
        assertNotNull(savedEntity.getId());
        dao.delete(savedEntity);
        var result = dao.findById(savedEntity.getId());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void findByDefaultViewId() {
        Long viewId = 23143L;
        testData.setDefaultViewId(viewId);
        var savedEntity = dao.save(testData);
        assertNotNull(savedEntity);
        assertNotNull(savedEntity.getId());
        var result = dao.findByDefaultViewId(viewId);
        assertNotNull(result);
        assertEquals(result.get().getId(), savedEntity.getId());
    }

    @Test
    void findByUsername() {
        String testUser = "user";
        testData.setUsername(testUser);
        var savedEntity = dao.save(testData);
        assertNotNull(savedEntity);
        var result = dao.findByUsername(testUser);
        assertNotNull(result);
        assertEquals(result.get().getId(), savedEntity.getId());
    }
}