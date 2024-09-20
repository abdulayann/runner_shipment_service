package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class TenantSettingsServiceTest {


    @Mock
    private ModelMapper modelMapper;
    @Mock
    private IV1Service v1Service;


    @InjectMocks
    TenantSettingsService tenantSettingsService;



    @Test
    void getV1TenantSettings() {
        Integer tenantId = 1;
        V1TenantSettingsResponse expectedTenantSettings = new V1TenantSettingsResponse();
        V1RetrieveResponse dependentServiceResponse =  new V1RetrieveResponse();
        dependentServiceResponse.setEntity(expectedTenantSettings);

        when(v1Service.retrieveTenantSettings()).thenReturn(dependentServiceResponse);
        when(modelMapper.map(any(), eq(V1TenantSettingsResponse.class))).thenReturn(expectedTenantSettings);

        var actual = tenantSettingsService.getV1TenantSettings(tenantId);

        assertEquals(expectedTenantSettings, actual);
    }

    @Test
    void getV1TenantSettingsReturnsNull() {
        Integer tenantId = 1;
        V1TenantSettingsResponse expectedTenantSettings = null;
        V1RetrieveResponse dependentServiceResponse =  null;

        when(v1Service.retrieveTenantSettings()).thenReturn(dependentServiceResponse);

        var actual = tenantSettingsService.getV1TenantSettings(tenantId);

        assertEquals(expectedTenantSettings, actual);
    }

}