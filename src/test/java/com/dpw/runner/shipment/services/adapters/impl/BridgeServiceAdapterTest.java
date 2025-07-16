package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BridgeServiceConfig;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.bridgeService.AuthLoginRequest;
import com.dpw.runner.shipment.services.dto.request.bridgeService.BridgeRequest;
import com.dpw.runner.shipment.services.dto.request.bridgeService.TactBridgePayload;
import com.dpw.runner.shipment.services.dto.response.bridgeService.AuthLoginResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
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

    @Test
    void testRequestOutBoundFileTransfer() throws RunnerException {
        BridgeRequest request = BridgeRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");
        BridgeServiceResponse bridgeServiceResponse = new BridgeServiceResponse();

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
                .thenReturn(ResponseEntity.ok(authLoginResponse));
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), eq(BridgeServiceResponse.class)))
                .thenReturn(ResponseEntity.ok(bridgeServiceResponse));

        var res = bridgeServiceAdapter.requestOutBoundFileTransfer(commonRequestModel);

        assertEquals(bridgeServiceResponse, res);
    }

    @Test
    void testRequestOutBoundFileTransfer_Exception() {
        BridgeRequest request = BridgeRequest.builder().requestCode("GBLCS").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
                .thenReturn(ResponseEntity.ok(authLoginResponse));
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), eq(BridgeServiceResponse.class)))
                .thenThrow(new RuntimeException("error"));

        assertThrows(RunnerException.class, () ->
                bridgeServiceAdapter.requestOutBoundFileTransfer(commonRequestModel));
    }


    @ParameterizedTest(name = "{index} => description={1}")
    @MethodSource("errorMessageProvider")
    @DisplayName("Should correctly extract description from error response")
    void testExtractErrorDescription_Parameterized(String errorMessage, String expectedDescription) {
        BridgeRequest request = BridgeRequest.builder().requestCode("GBLCS").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
                .thenReturn(ResponseEntity.ok(authLoginResponse));

        RestClientException exception = new RestClientException(errorMessage);

        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BridgeServiceResponse.class)))
                .thenThrow(exception);

        RunnerException thrownException = assertThrows(RunnerException.class, () -> {
            bridgeServiceAdapter.requestOutBoundFileTransfer(commonRequestModel);
        });

        assertEquals(expectedDescription, thrownException.getMessage());
    }

    private static Stream<Arguments> errorMessageProvider() {
        return Stream.of(
                Arguments.of("400: [{\"error\":{\"description\":\"Invalid request parameters\"}}]", "Invalid request parameters"),
                Arguments.of("400: [{\"error\":{\"code\":\"ERR001\"}}]", "Description not found"),
                Arguments.of("500: [{\"error\":{\"description\":\"Database connection failed\"}}, {\"error\":{\"description\":\"Secondary error\"}}]", "Database connection failed"),
                Arguments.of("400: [{\"message\":\"Something went wrong\"}]", "Description not found"),
                Arguments.of("400 Bad Request", "Invalid message format"),
                Arguments.of("400: [{\"error\":{\"description\":null}}]", "null"),
                Arguments.of("400: [{\"error\":{\"description\":\"\"}}]", "")
        );
    }

    @Test
    @DisplayName("Should return error parsing message when JSON is malformed")
    void testExtractErrorDescription_MalformedJson() {

        BridgeRequest request = BridgeRequest.builder().requestCode("GBLCS").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
                .thenReturn(ResponseEntity.ok(authLoginResponse));
        // Arrange
        String errorMessage = "400: [{\"error\":{\"description\":\"Invalid}]";
        RestClientException exception = new RestClientException(errorMessage);

        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BridgeServiceResponse.class)))
                .thenThrow(exception);

        // Act & Assert
        RunnerException thrownException = assertThrows(RunnerException.class, () -> {
            bridgeServiceAdapter.requestOutBoundFileTransfer(commonRequestModel);
        });

        assertTrue(thrownException.getMessage().startsWith("Error parsing detailMessage:"));
    }

    @Test
    @DisplayName("Should handle null exception message")
    void testExtractErrorDescription_NullExceptionMessage() {

        BridgeRequest request = BridgeRequest.builder().requestCode("GBLCS").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
                .thenReturn(ResponseEntity.ok(authLoginResponse));
        // Arrange
        RestClientException exception = new RestClientException((String) null);

        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BridgeServiceResponse.class)))
                .thenThrow(exception);

        // Act & Assert
        RunnerException thrownException = assertThrows(RunnerException.class, () -> {
            bridgeServiceAdapter.requestOutBoundFileTransfer(commonRequestModel);
        });

        assertTrue(thrownException.getMessage().startsWith("Error parsing detailMessage:"));
    }

    @Test
    @DisplayName("Should successfully return response when no exception occurs")
    void testSuccessfulRequest_ShouldReturnResponse() throws RunnerException {

        BridgeRequest request = BridgeRequest.builder().requestCode("GBLCS").build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);

        AuthLoginResponse authLoginResponse = new AuthLoginResponse();
        authLoginResponse.setAccessToken("accessToken");

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), eq(AuthLoginResponse.class)))
                .thenReturn(ResponseEntity.ok(authLoginResponse));
        // Arrange
        BridgeServiceResponse expectedResponse = new BridgeServiceResponse();
        ResponseEntity<BridgeServiceResponse> responseEntity = ResponseEntity.ok(expectedResponse);

        when(restTemplate.postForEntity(anyString(), any(HttpEntity.class), eq(BridgeServiceResponse.class)))
                .thenReturn(responseEntity);

        // Act
        IRunnerResponse result = bridgeServiceAdapter.requestOutBoundFileTransfer(commonRequestModel);

        // Assert
        assertEquals(expectedResponse, result);
        verify(restTemplate).postForEntity(anyString(), any(HttpEntity.class), eq(BridgeServiceResponse.class));
    }

}