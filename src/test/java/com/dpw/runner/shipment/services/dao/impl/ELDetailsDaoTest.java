package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dao.interfaces.IELDetailsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ELDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@RunWith(SpringRunner.class)
@ExtendWith(MockitoExtension.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
class ELDetailsDaoTest {

    @Autowired
    private IELDetailsDao dao;

    private static JsonTestUtility jsonTestUtility;
    private static ELDetails testData;

    @Container
    private static final PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");


    static {
        postgresContainer.withDatabaseName("integration-tests-db")
                .withUsername("sa")
                .withPassword("sa");
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
        testData = jsonTestUtility.getTestELDetails();
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
    void saveAll() {
        var result = dao.saveAll(List.of(testData));
        assertNotNull(result);
        assertNotNull(result.get(0).getId());
    }

    @Test
    void findByGuid() {
        var savedDetails = dao.save(testData);
        assertNotNull(savedDetails);
        var result = dao.findByGuid(savedDetails.getGuid());
        assertNotNull(result);
        assertTrue(result.isPresent());
        assertEquals(result.get().getId(), savedDetails.getId());
    }

    @Test
    void findAll() {
        testData.setShipmentId(1L);
        var result = dao.save(testData);
        Specification<ELDetails> spec = (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("shipmentId"), 1L);
        };
        var details = dao.findAll(spec, PageRequest.of(0 , 10));
        assertFalse(details.isEmpty());
        assertEquals(details.stream().toList().get(0).getShipmentId(), result.getShipmentId());

    }

    @Test
    void findById() {
        var savedDetails = dao.save(testData);
        assertNotNull(savedDetails);
        var result = dao.findById(savedDetails.getId());
        assertNotNull(result);
        assertTrue(result.isPresent());
        assertEquals(result.get().getId(), savedDetails.getId());
    }

    @Test
    void delete() {
        var savedDetails = dao.save(testData);
        dao.delete(savedDetails);
        var result = dao.findById(savedDetails.getId());
        assertTrue(result.isEmpty());
    }

    @Test
    void findByElNumber() {
        testData.setElNumber("TEST");
        var savedDetails = dao.save(testData);
        assertNotNull(savedDetails);
        var result = dao.findByElNumber(savedDetails.getElNumber());
        assertNotNull(result);
        assertTrue(result.isPresent());
        assertEquals(result.get(), savedDetails);
    }

    @Test
    void updateEntityFromShipment() throws RunnerException {
        testData.setShipmentId(5L);
        var details = List.of(testData);
        var result = dao.updateEntityFromShipment(details, testData.getShipmentId());
        assertFalse(result.isEmpty());
        assertNotNull(result.get(0).getShipmentId());
    }

    @Test
    void saveEntityFromShipment() {
        testData.setShipmentId(4L);
        var details = List.of(testData);
        var result = dao.saveEntityFromShipment(details, testData.getShipmentId());
        assertFalse(result.isEmpty());
        assertNotNull(result.get(0).getShipmentId());
    }
}