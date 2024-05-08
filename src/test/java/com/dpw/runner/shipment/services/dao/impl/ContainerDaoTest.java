//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
//import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
//import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
//import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
//import com.dpw.runner.shipment.services.dao.interfaces.IShipmentsContainersMappingDao;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
//import com.dpw.runner.shipment.services.entity.Containers;
//import com.dpw.runner.shipment.services.entity.ShipmentDetails;
//import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
//import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
//import com.dpw.runner.shipment.services.helper.JsonTestUtility;
//import com.fasterxml.jackson.databind.ObjectMapper;
//import org.junit.jupiter.api.*;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.runner.RunWith;
//import org.mockito.MockitoAnnotations;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.data.domain.PageRequest;
//import org.springframework.data.jpa.domain.Specification;
//import org.springframework.test.context.DynamicPropertyRegistry;
//import org.springframework.test.context.DynamicPropertySource;
//import org.springframework.test.context.TestPropertySource;
//import org.springframework.test.context.junit4.SpringRunner;
//import org.testcontainers.containers.PostgreSQLContainer;
//import org.testcontainers.junit.jupiter.Container;
//import org.testcontainers.junit.jupiter.Testcontainers;
//
//import java.io.IOException;
//import java.util.Collections;
//import java.util.List;
//import java.util.Map;
//
//import static org.junit.jupiter.api.Assertions.*;
//
//@RunWith(SpringRunner.class)
//@ExtendWith(MockitoExtension.class)
//@TestPropertySource("classpath:application-test.properties")
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@Testcontainers
//class ContainerDaoTest {
//
//    @Autowired
//    private IContainerDao containerDao;
//
//    @Autowired
//    private IShipmentsContainersMappingDao shipmentsContainersMappingDao;
//
//    @Autowired
//    private IConsolidationDetailsDao consolidationDetailsDao;
//
//    @Autowired
//    private IShipmentDao shipmentDao;
//
//    private static JsonTestUtility jsonTestUtility;
//    private static ShipmentDetails testShipment;
//    private static ConsolidationDetails testConsol;
//    private static Containers container;
//
//    private static ObjectMapper objectMapperTest;
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
//    @BeforeAll
//    static void beforeAll() throws IOException {
//        postgresContainer.start();
//        objectMapperTest = JsonTestUtility.getMapper();
//        jsonTestUtility = new JsonTestUtility();
//    }
//
//    @BeforeEach
//    void setUp() {
//        MockitoAnnotations.openMocks(this);
//        TenantContext.setCurrentTenant(1);
//        testShipment = jsonTestUtility.getTestShipment();
//        testConsol = jsonTestUtility.getTestNewConsolidation();
//        container = jsonTestUtility.getTestContainer();
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
//        var result = containerDao.save(container);
//        assertTrue(result.getId() != null);
//    }
//
//    @Test
//    void findAll() {
//        var result = containerDao.save(container);
//        Specification<Containers> spec =  (root, query, criteriaBuilder) ->
//            criteriaBuilder.equal(root.get("consolidationId"), 1);
//
//        var containerList = containerDao.findAll(spec, PageRequest.of(0 , 10));
//        assertFalse(containerList.isEmpty());
//        assertEquals(containerList.stream().toList().get(0).getContainerCode(), result.getContainerCode());
//    }
//
//    @Test
//    void getAllContainers() {
//        containerDao.save(container);
//        var containerList = containerDao.getAllContainers();
//        assertFalse(containerList.isEmpty());
//        assertTrue(containerList.get(0).getId() != null);
//    }
//
//    @Test
//    void findById() {
//        var savedContainer = containerDao.save(container);
//        var result = containerDao.findById(savedContainer.getId());
//        assertFalse(result.isEmpty());
//        assertEquals(result.get().getId() , savedContainer.getId());
//    }
//
//    @Test
//    void findByGuid() {
//        var savedContainer = containerDao.save(container);
//        var result = containerDao.findByGuid(savedContainer.getGuid());
//        assertFalse(result.isEmpty());
//        assertEquals(result.get(0).getId() , savedContainer.getId());
//    }
//
//    @Test
//    void delete() {
//        var savedContainer = containerDao.save(container);
//        containerDao.delete(savedContainer);
//        var result = containerDao.findById(savedContainer.getId());
//        assertTrue(result.isEmpty());
//    }
//
//    @Test
//    void deleteById() {
//        var savedContainer = containerDao.save(container);
//        containerDao.deleteById(savedContainer.getId());
//        var result = containerDao.findById(savedContainer.getId());
//        assertTrue(result.isEmpty());
//    }
//
//    @Test
//    void saveAll() {
//        var containersList = List.of(container);
//        var result = containerDao.saveAll(containersList);
//        assertFalse(result.isEmpty());
//        assertNotNull(result.get(0).getGuid());
//    }
//
//    @Test
//    void updateEntityFromConsolidationV1() throws RunnerException {
//        containerDao.save(container);
//        container.setContainerComments("New container");
//        var result = containerDao.updateEntityFromConsolidationV1(List.of(container) , container.getConsolidationId() , Collections.EMPTY_LIST);
//        assertFalse(result.isEmpty());
//    }
//
//    @Test
//    void updateEntityFromShipmentV1() throws RunnerException {
//        containerDao.save(container);
//        var result = containerDao.updateEntityFromShipmentV1(List.of(container) , Collections.EMPTY_LIST);
//        assertFalse(result.isEmpty());
//    }
//
//    @Test
//    @Disabled
//    void findByShipmentId() throws RunnerException {
//        var containerSaved = containerDao.save(container);
//        container.setShipmentsList(List.of(testShipment));
//        testShipment.setId(null);
//        testShipment.setGuid(null);
//        var shipment = shipmentDao.save(testShipment, false);
//        shipmentsContainersMappingDao.assignContainers(shipment.getId() , List.of(containerSaved.getId()));
//        var result = containerDao.findByShipmentId(shipment.getId());
//        assertFalse(result.isEmpty());
//    }
//
//    @Test
//    void findByConsolidationId() {
//        var consol = consolidationDetailsDao.save(testConsol , false);
//        container.setConsolidationId(consol.getId());
//        var containerSaved = containerDao.save(container);
//        var result = containerDao.findByConsolidationId(consol.getId());
//        assertFalse(result.isEmpty());
//    }
//}
