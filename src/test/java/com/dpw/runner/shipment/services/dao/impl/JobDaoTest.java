package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dao.interfaces.IJobDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Jobs;
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
class JobDaoTest {

    @Autowired
    private IJobDao jobDao;

    private static Jobs testJob;
    private static JsonTestUtility jsonTestUtility;

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
        testJob = jsonTestUtility.getTestJob();
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
        var result = jobDao.save(testJob);
        assertNotNull(result);
        assertNotNull(result.getId());
    }

    @Test
    void saveAll() {
        var result = jobDao.saveAll(List.of(testJob));
        assertNotNull(result);
        assertNotNull(result.get(0).getId());
    }

    @Test
    void findAll() {
        testJob.setConsolidationId(1L);
        var result = jobDao.save(testJob);
        Specification<Jobs> spec = (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("consolidationId"), 1);
        };
        var jobs = jobDao.findAll(spec, PageRequest.of(0 , 10));
        assertFalse(jobs.isEmpty());
        assertEquals(jobs.stream().toList().get(0).getConsolidationId(), result.getConsolidationId());
    }

    @Test
    void findById() {
        var savedJob = jobDao.save(testJob);
        var result = jobDao.findById(savedJob.getId());
        assertFalse(result.isEmpty());
        assertEquals(result.get().getId(), savedJob.getId());
    }

    @Test
    void delete() {
        var savedJob = jobDao.save(testJob);
        jobDao.delete(savedJob);
        var result = jobDao.findById(savedJob.getId());
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment() throws RunnerException {
        testJob.setConsolidationId(1L);
        testJob.setShipmentId(1L);
        var jobs = List.of(testJob);
        var result = jobDao.updateEntityFromShipment(jobs, testJob.getShipmentId());
        assertFalse(result.isEmpty());
        assertNotNull(result.get(0).getConsolidationId());
    }

    @Test
    void saveEntityFromShipment() {
        testJob.setConsolidationId(1L);
        testJob.setShipmentId(1L);
        var jobs = List.of(testJob);
        var result = jobDao.saveEntityFromShipment(jobs, testJob.getShipmentId());
        assertFalse(result.isEmpty());
        assertNotNull(result.get(0).getConsolidationId());
    }

    @Test
    void updateEntityFromConsole() throws RunnerException {
        testJob.setConsolidationId(1L);
        testJob.setShipmentId(1L);
        var jobs = List.of(testJob);
        var result = jobDao.updateEntityFromConsole(jobs, testJob.getConsolidationId());
        assertFalse(result.isEmpty());
        assertNotNull(result.get(0).getConsolidationId());
    }

    @Test
    void saveEntityFromConsole() {
        testJob.setConsolidationId(1L);
        testJob.setShipmentId(1L);
        var jobs = List.of(testJob);
        var result = jobDao.saveEntityFromConsole(jobs, testJob.getConsolidationId());
        assertFalse(result.isEmpty());
        assertNotNull(result.get(0).getShipmentId());
    }
}