package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ApprovalPartiesRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@PropertySource("classpath:application-qa.properties")
class MDMServiceAdapterTest {
    @Mock
    private RestTemplate restTemplate;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ObjectMapper objectMapper;

    @InjectMocks
    private MDMServiceAdapter mdmServiceAdapter;

    private String baseUrl = "http://localhost:8080";
    private String creditDetailsUrl = "/credit-details";

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        mdmServiceAdapter = new MDMServiceAdapter(restTemplate, baseUrl, creditDetailsUrl);
        mdmServiceAdapter.jsonHelper = this.jsonHelper;
        mdmServiceAdapter.objectMapper = this.objectMapper;
    }

    @Test
    void testGetCreditInfo_Success() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        Map<String, Object> responseBody = Map.of("key", "value");
        ResponseEntity<Object> responseEntity = new ResponseEntity<>(responseBody, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);

        ResponseEntity<IRunnerResponse> result = mdmServiceAdapter.getCreditInfo(commonRequestModel);

        assertNotNull(result);
        assertEquals(HttpStatus.OK, result.getStatusCode());
    }

    @Test
    void testGetCreditInfo_Exception() {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenThrow(new RuntimeException("Test Exception"));

        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            mdmServiceAdapter.getCreditInfo(commonRequestModel);
        });

        assertEquals("Error from MDM while fetching credit limit: Test Exception", exception.getMessage());
    }

    @Test
    void testGetApprovalStatusForParties_Success() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        LinkedHashMap<String, Object> dataMap = new LinkedHashMap<>();
        dataMap.put("data", List.of(Map.of("finalStatus", "APPROVED")));

        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(dataMap);

        ResponseEntity<Object> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);
        when(objectMapper.convertValue(any(), eq(DependentServiceResponse.class))).thenReturn(dependentServiceResponse);

        String result = mdmServiceAdapter.getApprovalStausForParties(commonRequestModel);

        assertNotNull(result);
        assertEquals("APPROVED", result);
    }

    @Test
    void testGetApprovalStatusForParties_NullData() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        LinkedHashMap<String, Object> dataMap = new LinkedHashMap<>();
        dataMap.put("data", null);

        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(dataMap);

        ResponseEntity<Object> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);
        when(objectMapper.convertValue(any(), eq(DependentServiceResponse.class))).thenReturn(dependentServiceResponse);

        String result = mdmServiceAdapter.getApprovalStausForParties(commonRequestModel);

        assertNull(result);
    }

    @Test
    void testGetApprovalStatusForParties_Exception() throws RunnerException {
        ApprovalPartiesRequest approvalPartiesRequest = new ApprovalPartiesRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.builder().data(approvalPartiesRequest).build();


        LinkedHashMap<String, Object> dataMap = new LinkedHashMap<>();
        dataMap.put("data", List.of(Map.of("finalStatus", "APPROVED")));

        DependentServiceResponse dependentServiceResponse = new DependentServiceResponse();
        dependentServiceResponse.setData(dataMap);

        ResponseEntity<Object> responseEntity = new ResponseEntity<>(dependentServiceResponse, HttpStatus.OK);

        when(jsonHelper.convertToJson(any())).thenReturn("{}");
        when(restTemplate.exchange(any(RequestEntity.class), eq(Object.class))).thenReturn(responseEntity);
        when(objectMapper.convertValue(any(), eq(DependentServiceResponse.class))).thenThrow(new RuntimeException("Test Exception"));

        String result = mdmServiceAdapter.getApprovalStausForParties(commonRequestModel);

        assertNull(result);
    }
}