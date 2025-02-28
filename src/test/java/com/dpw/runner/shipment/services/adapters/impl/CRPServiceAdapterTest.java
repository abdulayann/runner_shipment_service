package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.reportingservice.Models.DocumentRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
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
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {CRPServiceAdapter.class, String.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
class CRPServiceAdapterTest {
    @Autowired
    private CRPServiceAdapter cRPServiceAdapter;

    @MockBean(name = "restTemplateForCRP")
    private RestTemplate restTemplate;

    @Test
    void testRetrieveCRPService() throws RunnerException, RestClientException {

        // Arrange
        RestTemplate restTemplate = mock(RestTemplate.class);
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CRPServiceAdapter crpServiceAdapter = new CRPServiceAdapter(restTemplate, "https://example.org/example",
                "https://example.org/example");
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        CRPRetrieveRequest data = CRPRetrieveRequest.builder().build();
        requestModel.setData(data);

        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveCRPServiceResult = crpServiceAdapter.retrieveCRPService(requestModel);

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualRetrieveCRPServiceResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualRetrieveCRPServiceResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualRetrieveCRPServiceResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualRetrieveCRPServiceResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualRetrieveCRPServiceResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualRetrieveCRPServiceResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualRetrieveCRPServiceResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualRetrieveCRPServiceResult.getBody()).isSuccess());
        assertTrue(actualRetrieveCRPServiceResult.hasBody());
        assertTrue(actualRetrieveCRPServiceResult.getHeaders().isEmpty());
    }

    @Test
    void testRetrieveCRPService2() throws RunnerException {
        RestTemplate restTemplate = mock(RestTemplate.class);

        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.NOT_FOUND));
        CRPServiceAdapter crpServiceAdapter = new CRPServiceAdapter(restTemplate, "https://example.org/example",
                "https://example.org/example");
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        CRPRetrieveRequest data = CRPRetrieveRequest.builder().build();
        requestModel.setData(data);

        // Act
        ResponseEntity<IRunnerResponse> actualRetrieveCRPServiceResult = crpServiceAdapter.retrieveCRPService(requestModel);
        assertEquals(HttpStatus.OK, actualRetrieveCRPServiceResult.getStatusCode());
    }

    @Test
    void testListCRPService() throws RunnerException, RestClientException {

        // Arrange
        RestTemplate restTemplate = mock(RestTemplate.class);
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CRPServiceAdapter crpServiceAdapter = new CRPServiceAdapter(restTemplate, "https://example.org/example",
                "https://example.org/example");
        CRPListRequest data = CRPListRequest.builder().pageable(null).build();
        data.setBillable(false);
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        requestModel.setData(data);

        // Act
        ResponseEntity<IRunnerResponse> actualListCRPServiceResult = crpServiceAdapter.listCRPService(requestModel);

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualListCRPServiceResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualListCRPServiceResult.getBody()).isSuccess());
        assertTrue(actualListCRPServiceResult.hasBody());
        assertTrue(actualListCRPServiceResult.getHeaders().isEmpty());
    }

    @Test
    void testListCRPService2() throws RunnerException, RestClientException {

        // Arrange
        RestTemplate restTemplate = mock(RestTemplate.class);
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.SWITCHING_PROTOCOLS));
        CRPServiceAdapter crpServiceAdapter = new CRPServiceAdapter(restTemplate, "https://example.org/example",
                "https://example.org/example");
        CRPListRequest data = CRPListRequest.builder().pageable(null).build();
        data.setBillable(false);
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        requestModel.setData(data);

        // Act
        ResponseEntity<IRunnerResponse> actualListCRPServiceResult = crpServiceAdapter.listCRPService(requestModel);

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualListCRPServiceResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualListCRPServiceResult.getBody()).isSuccess());
        assertTrue(actualListCRPServiceResult.hasBody());
        assertTrue(actualListCRPServiceResult.getHeaders().isEmpty());
    }

    @Test
    void testListCRPService3() throws RunnerException, RestClientException {

        // Arrange
        RestTemplate restTemplate = mock(RestTemplate.class);
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        CRPServiceAdapter crpServiceAdapter = new CRPServiceAdapter(restTemplate, "https://example.org/example",
                "https://example.org/example");
        CRPListRequest data = CRPListRequest.builder().pageable(null).build();
        data.setBillable(true);
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        requestModel.setData(data);

        // Act
        ResponseEntity<IRunnerResponse> actualListCRPServiceResult = crpServiceAdapter.listCRPService(requestModel);

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualListCRPServiceResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualListCRPServiceResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualListCRPServiceResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualListCRPServiceResult.getBody()).isSuccess());
        assertTrue(actualListCRPServiceResult.hasBody());
        assertTrue(actualListCRPServiceResult.getHeaders().isEmpty());
    }

    @Test
    void testListCRPService4() throws RunnerException, RestClientException {

        // Arrange
        RestTemplate restTemplate = mock(RestTemplate.class);
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        CRPServiceAdapter crpServiceAdapter = new CRPServiceAdapter(restTemplate, "https://example.org/example",
                "https://example.org/example");
        CRPListRequest data = CRPListRequest.builder().pageable(null).build();
        data.setBillable(false);
        CommonRequestModel.CommonRequestModelBuilder builderResult = CommonRequestModel.builder();
        CommonRequestModel.CommonRequestModelBuilder dataResult = builderResult.data(new DocumentRequest());
        CommonRequestModel requestModel = dataResult.dataList(new ArrayList<>())
                .dependentData("Dependent Data")
                .guid("1234")
                .id(1L)
                .build();
        requestModel.setData(data);

        ResponseEntity<IRunnerResponse> actualRetrieveCRPServiceResult = crpServiceAdapter.listCRPService(requestModel);
        assertEquals(HttpStatus.OK, actualRetrieveCRPServiceResult.getStatusCode());
    }
}
