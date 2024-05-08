package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dao.interfaces.IPackingDao;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@RunWith(SpringRunner.class)
@ExtendWith(MockitoExtension.class)
@TestPropertySource("classpath:application-test.properties")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
class PackingDaoTest {

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

    @Autowired
    private IPackingDao packingDao;

    private static JsonTestUtility jsonTestUtility;
    private static Packing testPacking;

    private static ObjectMapper objectMapperTest;
    private static PackingRequest testPackingRequest;
    private static PackingResponse testPackingResponse;

    static {
        try {
            objectMapperTest = JsonTestUtility.getMapper();
            jsonTestUtility = new JsonTestUtility();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        testPacking = jsonTestUtility.getTestPacking();
        testPackingRequest = objectMapperTest.convertValue(testPacking , PackingRequest.class);
        testPackingResponse = objectMapperTest.convertValue(testPacking , PackingResponse.class);
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }


    @Test
    void save() {
        var result = packingDao.save(testPacking);
        assertTrue(result.getId() != null);
    }

    @Test
    void findAll() {
        testPacking.setConsolidationId(1L);
        var result = packingDao.save(testPacking);
        Specification<Packing> spec = (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("consolidationId"), 1);
        };
        var packingList = packingDao.findAll(spec, PageRequest.of(0 , 10));
        assertFalse(packingList.isEmpty());
        assertEquals(packingList.stream().toList().get(0).getConsolidationId(), result.getConsolidationId());
    }

    @Test
    void findById() {
        var savedPacking = packingDao.save(testPacking);
        var result = packingDao.findById(savedPacking.getId());
        assertFalse(result.isEmpty());
        assertEquals(result.get().getId() , savedPacking.getId());
    }

    @Test
    void findByGuid() {
        var savedPacking = packingDao.save(testPacking);
        var result = packingDao.findByGuid(savedPacking.getGuid());
        assertFalse(result.isEmpty());
        assertEquals(result.get().getId() , savedPacking.getId());
    }

    @Test
    void delete() {
        var savedPacking = packingDao.save(testPacking);
        packingDao.delete(savedPacking);
        var result = packingDao.findById(savedPacking.getId());
        assertTrue(result.isEmpty());
    }

    @Test
    void updateEntityFromShipment() throws RunnerException {
        testPacking.setContainerId(12L);
        testPacking.setShipmentId(1L);
        packingDao.save(testPacking);
        var packings = List.of(testPacking);
        var result = packingDao.updateEntityFromShipment(packings, testPacking.getShipmentId(), List.of(12L));
        assertFalse(result.isEmpty());
        assertNull(result.get(0).getContainerId());
    }

    @Test
    void getAllPackings() {
        testPacking.setConsolidationId(19L);
        packingDao.save(testPacking);
        var packingList = packingDao.getAllPackings();
        assertFalse(packingList.isEmpty());
        assertEquals(packingList.stream().toList().stream().anyMatch(c -> c.getContainerId() == testPacking.getContainerId()), true);
    }

    @Test
    void saveAll() {
        var containersList = List.of(testPacking);
        var result = packingDao.saveAll(containersList);
        assertFalse(result.isEmpty());
        assertNotNull(result.get(0).getGuid());
    }

    @Test
    void saveEntityFromShipment() {
        testPacking.setShipmentId(1L);
        packingDao.save(testPacking);
        var packings = List.of(testPacking);
        var result = packingDao.saveEntityFromShipment(packings, testPacking.getShipmentId());
        assertNotNull(result);
    }

    @Test
    void insertContainerInPacking() throws RunnerException {
        testPacking.setId(null);
        var savedPack = packingDao.save(testPacking);
        var result = packingDao.insertContainerInPacking(List.of(savedPack) , 1L);
        assertNotNull(result);
        assertFalse(result.isEmpty());
        assertEquals(result.get(0).getContainerId() , 1L);
    }

    @Test
    void saveEntityFromContainer() {
        Long containerId = 12L;
        var savedPack = packingDao.save(testPacking);
        var result = packingDao.saveEntityFromContainer(List.of(savedPack), containerId);
        assertFalse(result.isEmpty());
        assertEquals(result.get(0).getContainerId(), containerId);
    }

    @Test
    void removeEntityFromContainer() throws RunnerException {
        Long containerId = 12L;
        testPacking.setContainerId(containerId);
        var packings = List.of(testPacking);
        var result = packingDao.removeEntityFromContainer(packings, containerId, Collections.emptyList());
        assertFalse(result.isEmpty());
        assertNull(result.get(0).getContainerId());
    }


    @Test
    void deleteEntityFromContainer() {
        Long containerId = 12L;
        testPacking.setContainerId(containerId);
        var savedPack = packingDao.save(testPacking);
        packingDao.deleteEntityFromContainer(containerId);
        var result = packingDao.findById(savedPack.getId());
        assertEquals(result.get().getGuid(), testPacking.getGuid());
        assertNull(result.get().getContainerId());
    }

    @Test
    void findByConsolidationId() {
        testPacking.setConsolidationId(1982L);
        var savedPacking = packingDao.save(testPacking);
        var result = packingDao.findByConsolidationId(savedPacking.getConsolidationId());
        assertNotNull(result);
        assertEquals(savedPacking.getConsolidationId(), result.get(0).getConsolidationId());
        assertEquals(savedPacking.getId(), result.get(0).getId());
    }
}
