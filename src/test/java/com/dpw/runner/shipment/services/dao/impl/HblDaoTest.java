//package com.dpw.runner.shipment.services.dao.impl;
//
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
//import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
//import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
//import com.dpw.runner.shipment.services.dto.request.UsersDto;
//import com.dpw.runner.shipment.services.entity.Hbl;
//import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
//import com.dpw.runner.shipment.services.helper.JsonTestUtility;
//import org.junit.jupiter.api.*;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.jupiter.api.parallel.Execution;
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
//import javax.transaction.Transactional;
//import java.io.IOException;
//import java.util.HashMap;
//import java.util.List;
//import java.util.Optional;
//
//import static org.junit.jupiter.api.Assertions.*;
//import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
//
//@RunWith(SpringRunner.class)
//@ExtendWith(MockitoExtension.class)
//@TestPropertySource("classpath:application-test.properties")
//@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
//@Testcontainers
//@Execution(CONCURRENT)
//class HblDaoTest {
//
//    @Container
//    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");
//    private static JsonTestUtility jsonTestUtility;
//
//    static {
//        postgresContainer = new PostgreSQLContainer("postgres:15-alpine")
//                .withDatabaseName("integration-tests-db")
//                .withUsername("sa")
//                .withPassword("sa");
//        postgresContainer.start();
//    }
//
//    @Autowired
//    private IHblDao hblDao;
//
//    @BeforeAll
//    static void beforeAll() throws IOException {
//        postgresContainer.start();
//        jsonTestUtility = new JsonTestUtility();
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
//    @BeforeEach
//    void setUp() {
//        MockitoAnnotations.openMocks(this);
//        TenantContext.setCurrentTenant(1);
//        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
//        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
//    }
//
//
//    @Test
//    public void testSaveFailsWhenShipmentIdIsNull() {
//        Hbl hbl = new Hbl();
//
//        Exception e = assertThrows(Exception.class, () -> hblDao.save(hbl));
//    }
//
//    @Test
//    public void testSaveSuccess() {
//        Hbl hbl = new Hbl();
//        hbl.setShipmentId(1L);
//
//        Hbl savedHbl = hblDao.save(hbl);
//
//        assertNotNull(savedHbl);
//        assertEquals(1L, savedHbl.getId());
//    }
//
//    @Test
//    void findById() {
//        Hbl savedHbl = saveHbl();
//        Optional<Hbl> optional = hblDao.findById(savedHbl.getId());
//        assertTrue(optional.isPresent());
//        assertEquals(savedHbl.getId(), optional.get().getId());
//    }
//
//    @Test
//    void findByShipmentId() {
//        Hbl savedHBl = saveHbl();
//        List<Hbl> hblList = hblDao.findByShipmentId(savedHBl.getShipmentId());
//        assertNotNull(hblList);
//    }
//
//    @Test
//    void delete() {
//        Hbl savedHbl = saveHbl();
//        Optional<Hbl> optional = hblDao.findById(1L);
//        assertTrue(optional.isPresent());
//
//        deleteHbl(optional.get());
//
//        optional = hblDao.findById(1L);
//        assertTrue(optional.isEmpty());
//    }
//
//
//    @Transactional
//    Hbl saveHbl() {
//        Hbl hbl = new Hbl();
//        hbl.setShipmentId(1L);
//        return hblDao.save(hbl);
//    }
//
//    @Transactional
//    void deleteHbl(Hbl savedHbl) {
//        hblDao.delete(savedHbl);
//    }
//
//}
