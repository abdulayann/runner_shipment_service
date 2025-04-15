package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BridgeServiceConfig;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dto.request.bridgeService.AuthLoginRequest;
import com.dpw.runner.shipment.services.dto.request.bridgeService.TactBridgePayload;
import com.dpw.runner.shipment.services.dto.response.bridgeService.AuthLoginResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class BridgeServiceAdapterTest {

    @Mock
    private RestTemplate restTemplate;
    @Mock
    private BridgeServiceConfig bridgeServiceConfig;
    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    BridgeServiceAdapter bridgeServiceAdapter;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapper;


    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }


    @Test
    void testRequestTactResponse() throws RunnerException {
        TactBridgePayload tactBridgePayload = TactBridgePayload.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tactBridgePayload);


        AuthLoginRequest authLoginRequest = new AuthLoginRequest();
        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");
        BridgeServiceResponse bridgeServiceResponse = new BridgeServiceResponse();

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
            .thenReturn(ResponseEntity.ok(authLoginResponse));
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), eq(BridgeServiceResponse.class)))
            .thenReturn(ResponseEntity.ok(bridgeServiceResponse));

        var res = bridgeServiceAdapter.requestTactResponse(commonRequestModel);

        assertEquals(bridgeServiceResponse, res);
    }

    @Test
    void testRequestTactResponseTokenGenerateThrowsError() throws RunnerException {
        TactBridgePayload tactBridgePayload = TactBridgePayload.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tactBridgePayload);


        AuthLoginRequest authLoginRequest = new AuthLoginRequest();
        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");
        BridgeServiceResponse bridgeServiceResponse = new BridgeServiceResponse();

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
            .thenThrow(new RuntimeException("error"));

        assertThrows(RunnerException.class, () ->
            bridgeServiceAdapter.requestTactResponse(commonRequestModel));
    }

    @Test
    void testRequestTactResponseBridgeServiceThrowsError() throws RunnerException {
        TactBridgePayload tactBridgePayload = TactBridgePayload.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(tactBridgePayload);


        AuthLoginRequest authLoginRequest = new AuthLoginRequest();
        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");
        BridgeServiceResponse bridgeServiceResponse = new BridgeServiceResponse();

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
            .thenReturn(ResponseEntity.ok(authLoginResponse));
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), eq(BridgeServiceResponse.class)))
            .thenThrow(new RuntimeException("error"));

        assertThrows(RunnerException.class, () ->
            bridgeServiceAdapter.requestTactResponse(commonRequestModel));
    }

}