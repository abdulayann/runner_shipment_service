//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
//import com.dpw.runner.shipment.services.dto.request.IntegrationResponseRequest;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.entity.IntegrationResponse;
//import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
//import com.dpw.runner.shipment.services.helper.JsonTestUtility;
//import org.junit.jupiter.api.AfterAll;
//import org.junit.jupiter.api.BeforeAll;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.runner.RunWith;
//import org.mockito.MockitoAnnotations;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.test.context.DynamicPropertyRegistry;
//import org.springframework.test.context.DynamicPropertySource;
//import org.springframework.test.context.TestPropertySource;
//import org.springframework.test.context.junit4.SpringRunner;
//import org.testcontainers.containers.PostgreSQLContainer;
//import org.testcontainers.junit.jupiter.Container;
//import org.testcontainers.junit.jupiter.Testcontainers;
//
//import java.io.IOException;
//import java.util.List;
//import java.util.Map;
//
//import static org.junit.jupiter.api.Assertions.assertNotNull;
//
//@RunWith(SpringRunner.class)
//@ExtendWith(MockitoExtension.class)
//@TestPropertySource("classpath:application-test.properties")
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@Testcontainers
//class IntegrationDaoTest {
//
//    @Autowired
//    private IntegrationDao integrationDao;
//
//    private static JsonTestUtility jsonTestUtility;
//
//    private static IntegrationResponse testData;
//
//    @Container
//    private static final PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");
//
//    static {
//        postgresContainer.withDatabaseName("integration-tests-db")
//                .withUsername("sa")
//                .withPassword("sa");
//        postgresContainer.start();
//    }
//
//
//    @BeforeAll
//    static void beforeAll() throws IOException {
//        postgresContainer.start();
//        jsonTestUtility = new JsonTestUtility();
//    }
//
//    @BeforeEach
//    void setUp() {
//        MockitoAnnotations.openMocks(this);
//        TenantContext.setCurrentTenant(1);
//        testData = jsonTestUtility.getTestIntegrationResponse();
//        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
//        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
//        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
//        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
//    }
//
//    @AfterAll
//    static void afterAll() {
//        postgresContainer.stop();
//    }
//
//    @DynamicPropertySource
//    static void dynamicConfiguration(DynamicPropertyRegistry registry){
//        registry.add("spring.datasource.url", postgresContainer::getJdbcUrl);
//        registry.add("spring.datasource.username", postgresContainer::getUsername);
//        registry.add("spring.datasource.password", postgresContainer::getPassword);
//    }
//
//
//    @Test
//    void save() {
//        var result = integrationDao.save(testData);
//        assertNotNull(result.getId());
//    }
//
//    @Test
//    void getIntegrationResponses() {
//        integrationDao.save(testData);
//        IntegrationResponseRequest request = IntegrationResponseRequest
//                .builder().entityId(12345L).entityType("Product").build();
//        var result = integrationDao.getIntegrationResponses(request);
//        assertNotNull(result);
//        assertNotNull(result.get(0).getId());
//    }
//}