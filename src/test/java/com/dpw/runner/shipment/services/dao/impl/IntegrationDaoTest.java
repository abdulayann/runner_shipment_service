package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.IntegrationResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IIntegrationRespsonseRepository;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertNotNull;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class IntegrationDaoTest {

    @InjectMocks
    private IntegrationDao integrationDao;

    @Mock
    private IIntegrationRespsonseRepository integrationRespsonseRepository;

    private static JsonTestUtility jsonTestUtility;

    private static IntegrationResponse testData;

    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        testData = jsonTestUtility.getTestIntegrationResponse();
        var permissions = Map.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive" , true);
        PermissionsContext.setPermissions(List.of("Consolidations:Retrive:Sea Consolidation:AllSeaConsolidationRetrive"));
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(permissions).build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
    }

    @Test
    void save() {
        testData.setId(12L);
        Mockito.when(integrationRespsonseRepository.save(Mockito.any())).thenReturn(testData);
        var result = integrationDao.save(testData);
        assertNotNull(result.getId());
    }

    @Test
    void getIntegrationResponses() {
        testData.setId(12L);
        Mockito.when(integrationRespsonseRepository.findByEntityIdAndEntityType(Mockito.any(), Mockito.any())).thenReturn(Optional.of(List.of(testData)));
        IntegrationResponseRequest request = IntegrationResponseRequest
                .builder().entityId(12345L).entityType("Product").build();
        var result = integrationDao.getIntegrationResponses(request);
        assertNotNull(result);
        assertNotNull(result.get(0).getId());
    }
}