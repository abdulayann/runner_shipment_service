package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dao.interfaces.IAdditionalDetailDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
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
class AdditionalDetailDaoTest {

    @Autowired
    private IAdditionalDetailDao additionalDetailDao;

    private static AdditionalDetails testAdditionalDetails;
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
        testAdditionalDetails = jsonTestUtility.getTestAdditionalDetails();
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        var result = additionalDetailDao.save(testAdditionalDetails);
        assertNotNull(result);
        assertNotNull(result.getId());
    }

    @Test
    void findAll() {
        var result = additionalDetailDao.save(testAdditionalDetails);
        Specification<AdditionalDetails> spec =  (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("efreightStatus"), "EAW");

        var additionalDetailsPage = additionalDetailDao.findAll(spec, PageRequest.of(0 , 10));
        assertFalse(additionalDetailsPage.isEmpty());
        assertEquals(additionalDetailsPage.stream().toList().get(0).getEfreightStatus(), result.getEfreightStatus());
    }

    @Test
    void findById() {
        var savedDetails = additionalDetailDao.save(testAdditionalDetails);
        var result = additionalDetailDao.findById(testAdditionalDetails.getId());
        assertFalse(result.isEmpty());
        assertEquals(result.get().getId() , savedDetails.getId());
    }

    @Test
    void delete() {
        var savedDetails = additionalDetailDao.save(testAdditionalDetails);
        additionalDetailDao.delete(savedDetails);
        var result = additionalDetailDao.findById(savedDetails.getId());
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment() throws RunnerException {
        var savedDetails = additionalDetailDao.updateEntityFromShipment(testAdditionalDetails);
        assertNotNull(savedDetails.getId());
    }
}