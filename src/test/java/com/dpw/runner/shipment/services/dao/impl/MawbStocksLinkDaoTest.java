package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksDao;
import com.dpw.runner.shipment.services.dao.interfaces.IMawbStocksLinkDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.MawbStocks;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import org.junit.jupiter.api.*;
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
class MawbStocksLinkDaoTest {

    @Autowired
    private IMawbStocksLinkDao dao;

    @Autowired
    private IMawbStocksDao mawbStocksDao;

    private static JsonTestUtility jsonTestUtility;
    private static MawbStocksLink testData;
    private static MawbStocks testSockData;
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
        testData = jsonTestUtility.getTestMawbStocksLink();
        testSockData = jsonTestUtility.getTestStockData();
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
        var r = mawbStocksDao.save(testSockData);
        testData.setParentId(r.getId());
        var result = dao.save(testData);
        assertNotNull(result);
        assertNotNull(result.getId());
    }

    @Test
    void findAll() {
        var r = mawbStocksDao.save(testSockData);
        testData.setParentId(r.getId());
        testData.setStatus("test");
        var result = dao.save(testData);
        Specification<MawbStocksLink> spec = (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("status"), "test");
        };
        var jobs = dao.findAll(spec, PageRequest.of(0 , 10));
        assertFalse(jobs.isEmpty());
        assertEquals(jobs.stream().toList().get(0).getStatus(), result.getStatus());
    }

    @Test
    void findByMawbNumber() {
        final String MAWB_NUM = "TEST";
        var r = mawbStocksDao.save(testSockData);
        testData.setMawbNumber(MAWB_NUM);
        testData.setParentId(r.getId());
        dao.save(testData);
        var result = dao.findByMawbNumber(MAWB_NUM);
        assertNotNull(result);
        assertFalse(result.isEmpty());
        assertTrue(result.get(0).getMawbNumber().equals(MAWB_NUM));
    }

    @Test
    void deleteByParentId() {
        final String MAWB_NUM = "TEST";
        var r = mawbStocksDao.save(testSockData);
        testData.setMawbNumber(MAWB_NUM);
        testData.setParentId(r.getId());
        dao.save(testData);
        dao.deleteByParentId(testData.getParentId());
        var result = dao.findByMawbNumber(testData.getMawbNumber());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void deLinkExistingMawbStockLink() {
        testData.setMawbNumber("TEST_NUM");
        var r = mawbStocksDao.save(testSockData);
        testData.setParentId(r.getId());
        String mawbNumber = "TEST_NUM";
        dao.save(testData);
        dao.deLinkExistingMawbStockLink(mawbNumber);
        var result = dao.findByMawbNumber(mawbNumber);
        assertNotNull(result);
        assertNull(result.get(0).getEntityId());
        assertNull(result.get(0).getEntityType());
    }

}