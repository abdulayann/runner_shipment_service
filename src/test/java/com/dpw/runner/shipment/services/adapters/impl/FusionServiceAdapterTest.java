package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.reportingservice.Models.DocumentRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {FusionServiceAdapter.class, String.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
class FusionServiceAdapterTest {
    @Autowired
    private FusionServiceAdapter fusionServiceAdapter;

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean(name = "restTemplateForCreditCheckP100")
    private RestTemplate restTemplate;

    @Test
    void testCheckCreditLimitP100() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<String>any())).thenReturn("Convert To Json");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        ResponseEntity<IRunnerResponse> actualCheckCreditLimitP100Result = fusionServiceAdapter
                .checkCreditLimitP100(requestModel);

        // Assert
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper).convertToJson(isNull());
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getError());
        assertNull(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getData());
        assertNull(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualCheckCreditLimitP100Result.getStatusCode());
        assertTrue(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).isSuccess());
        assertTrue(actualCheckCreditLimitP100Result.hasBody());
        assertTrue(actualCheckCreditLimitP100Result.getHeaders().isEmpty());
    }

    @Test
    void testCheckCreditLimitP100_2() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<String>any())).thenReturn("Convert To Json");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new RuntimeException("Fusion credit check failed due to: {}"));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(RuntimeException.class, () -> fusionServiceAdapter.checkCreditLimitP100(requestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<String>any());
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
    }

    @Test
    void testCheckCreditLimitP100_3() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<String>any())).thenReturn("Return Response with data {}");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act
        ResponseEntity<IRunnerResponse> actualCheckCreditLimitP100Result = fusionServiceAdapter
                .checkCreditLimitP100(requestModel);

        // Assert
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper).convertToJson(isNull());
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getError());
        assertNull(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getData());
        assertNull(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualCheckCreditLimitP100Result.getStatusCode());
        assertTrue(((DependentServiceResponse) actualCheckCreditLimitP100Result.getBody()).isSuccess());
        assertTrue(actualCheckCreditLimitP100Result.hasBody());
        assertTrue(actualCheckCreditLimitP100Result.getHeaders().isEmpty());
    }

    @Test
    void testCheckCreditLimitP100_4() throws RunnerException, RestClientException {
        // Arrange
        when(jsonHelper.convertToJson(Mockito.<String>any())).thenReturn("Convert To Json");
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any())).thenReturn(null);
        CommonRequestModel.CommonRequestModelBuilder commonRequestModelBuilder = mock(
                CommonRequestModel.CommonRequestModelBuilder.class);
        when(commonRequestModelBuilder.data(Mockito.<IRunnerRequest>any())).thenReturn(CommonRequestModel.builder());
        CommonRequestModel.CommonRequestModelBuilder dataResult = commonRequestModelBuilder.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();

        // Act and Assert
        assertThrows(RuntimeException.class, () -> fusionServiceAdapter.checkCreditLimitP100(requestModel));
        verify(commonRequestModelBuilder).data(isA(IRunnerRequest.class));
        verify(jsonHelper, atLeast(1)).convertToJson(Mockito.<String>any());
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
    }
}
