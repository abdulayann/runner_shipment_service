package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.dao.interfaces.IFileRepoDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.FileRepo;
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
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.utils.CommonUtils.constructListCommonRequest;
import static org.junit.jupiter.api.Assertions.*;

@RunWith(SpringRunner.class)
@ExtendWith(MockitoExtension.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
class FileRepoDaoTest {

    @Autowired
    private IFileRepoDao fileRepoDao;
    private static JsonTestUtility jsonTestUtility;
    private static FileRepo testData;

    @Container
    private static PostgreSQLContainer<?> postgresContainer = new PostgreSQLContainer<>("postgres:15-alpine");

    static {
        postgresContainer = new PostgreSQLContainer("postgres:15-alpine")
                .withDatabaseName("integration-tests-db")
                .withUsername("sa")
                .withPassword("sa");
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
        testData = jsonTestUtility.getTestFileRepoData();
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }


    @Test
    void save() {
        var r = fileRepoDao.save(testData);
        assertNotNull(r);
        assertNotNull(r.getId());
    }

    @Test
    void saveAll() {
        var r = fileRepoDao.saveAll(List.of(testData));
        assertNotNull(r);
        assertNotNull(r.get(0).getId());
    }

    @Test
    void findAll() {
        var r = fileRepoDao.save(testData);
        assertNotNull(r.getId());
        Specification<FileRepo> spec = (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("id"), r.getId());
        };
        var result = fileRepoDao.findAll(spec, PageRequest.of(0 , 10));
        assertNotNull(result);
        assertEquals(result.stream().toList().get(0).getId(), r.getId());
    }

    @Test
    void findById() {
        var r = fileRepoDao.save(testData);
        assertNotNull(r.getId());
        var result = fileRepoDao.findById(r.getId());
        assertNotNull(result);
        assertEquals(result.get(), r);
    }

    @Test
    void delete() {
        var r = fileRepoDao.save(testData);
        assertNotNull(r.getId());
        fileRepoDao.delete(r);
        var result = fileRepoDao.findById(r.getId());
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void findByEntityIdAndEntityType() {
        var r = fileRepoDao.save(testData);
        assertNotNull(r.getEntityId());
        assertNotNull(r.getEntityType());
        var result = fileRepoDao.findByEntityIdAndEntityType(r.getEntityId(), r.getEntityType());
        assertEquals(r , result.get(0));
    }

    @Test
    void findByList() {
        var r = fileRepoDao.save(testData);
        assertNotNull(r.getEntityId());
        assertNotNull(r.getEntityType());
        ListCommonRequest request = constructListCommonRequest("id", r.getId(), "=");
        var result = fileRepoDao.findByList(request);
        assertFalse(result.isEmpty());
        assertEquals(result.get(0), r);
    }
}