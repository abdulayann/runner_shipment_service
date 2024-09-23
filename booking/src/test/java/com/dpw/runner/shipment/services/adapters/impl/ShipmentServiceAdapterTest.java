package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.ShipmentServiceConfig;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class ShipmentServiceAdapterTest {
    @Mock
    private RestTemplate restTemplate;

    @Mock
    private IV1Service v1Service;

    @Mock
    private V2AuthHelper v2AuthHelper;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ShipmentServiceConfig shipmentServiceConfig;

    @InjectMocks
    private ShipmentServiceAdapter shipmentServiceAdapter;


    @BeforeEach
    void setUp() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void createShipmentInV2Test() throws RunnerException {
        CustomerBookingRequest customerBookingRequest = CustomerBookingRequest.builder().build();
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        ResponseEntity<ShipmentDetailsResponse> responseEntity = ResponseEntity.ok(shipmentDetailsResponse);
        when(restTemplate.postForEntity(any(String.class), any(HttpEntity.class), any(Class.class)))
                .thenReturn(responseEntity);
        ResponseEntity<IRunnerResponse> response = shipmentServiceAdapter.createShipmentInV2(customerBookingRequest);
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }


    @Test
    void getShipmentByGuidTest() throws RunnerException {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        when(restTemplate.exchange(eq("nullnull?guid=abcd"), eq(HttpMethod.GET), any(HttpEntity.class), any(Class.class)))
                .thenReturn(ResponseEntity.ok(shipmentDetailsResponse));

        ResponseEntity<IRunnerResponse> response = shipmentServiceAdapter.getShipmentIdbyGuid("abcd");
        assertEquals(HttpStatus.OK, response.getStatusCode());
    }

}
