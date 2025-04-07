package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.*;
import com.dpw.runner.shipment.services.dto.request.IntegrationResponseRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.IntegrationResponsesResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class IntegrationResponseServiceTest extends CommonMocks{
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IIntegrationResponseDao integrationResponseDao;
    @InjectMocks
    private IntegrationResponseService integrationResponseService;

    @BeforeEach
    void setUp() {
        UserContext.setUser(UsersDto.builder().Username("user").build());
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().isNetworkTransferEntityEnabled(Boolean.TRUE).build());
        TenantContext.setCurrentTenant(1);
    }

    @Test
    void testCreate(){
        var responseEntity = integrationResponseService.create(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertNull(responseEntity);
    }
    @Test
    void testUpdate(){
        var responseEntity = integrationResponseService.update(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertNull(responseEntity);
    }
    @Test
    void testList(){
        var responseEntity = integrationResponseService.list(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertNull(responseEntity);
    }
    @Test
    void testListAsync(){
        var responseEntity = integrationResponseService.listAsync(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertNull(responseEntity);
    }
    @Test
    void testDelete(){
        var responseEntity = integrationResponseService.delete(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertNull(responseEntity);
    }
    @Test
    void testRetrieveById(){
        var responseEntity = integrationResponseService.retrieveById(CommonRequestModel.buildRequest(CommonGetRequest.builder().build()));
        assertNull(responseEntity);
    }
    @Test
    void testFetchIntegrationResponses(){
        IntegrationResponse integrationResponse = IntegrationResponse.builder().build();
        when(integrationResponseDao.getIntegrationResponses(any())).thenReturn(Collections.singletonList(integrationResponse));
        when(jsonHelper.convertValue(integrationResponse, IntegrationResponsesResponse.class)).thenReturn(IntegrationResponsesResponse.builder().build());
        var responseEntity = integrationResponseService.fetchIntegrationResponses(CommonRequestModel.buildRequest(IntegrationResponseRequest.builder().build()));
        assertNotNull(responseEntity);
    }

}