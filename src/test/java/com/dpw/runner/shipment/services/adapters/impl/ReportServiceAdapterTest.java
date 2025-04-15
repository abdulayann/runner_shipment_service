package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.responses.ApiError;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dto.request.reportService.MailAuditLogRequest;
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

import java.net.URISyntaxException;
import java.util.HashMap;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.*;

@ContextConfiguration(classes = {ReportServiceAdapter.class, String.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
class ReportServiceAdapterTest {
    @Autowired
    private ReportServiceAdapter reportServiceAdapter;

    @MockBean(name = "restTemplateForReportService")
    private RestTemplate restTemplate;

    @Test
    void testPostRequest() throws RunnerException, URISyntaxException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        MailAuditLogRequest body = new MailAuditLogRequest();

        // Act
        ResponseEntity<IRunnerResponse> actualPostRequestResult = reportServiceAdapter.postRequest(body, new HashMap<>());

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualPostRequestResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualPostRequestResult.getBody()).isSuccess());
        assertTrue(actualPostRequestResult.hasBody());
        assertTrue(actualPostRequestResult.getHeaders().isEmpty());
    }

    @Test
    void testPostRequest2() throws RunnerException, URISyntaxException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.SWITCHING_PROTOCOLS));
        MailAuditLogRequest body = new MailAuditLogRequest();

        // Act
        ResponseEntity<IRunnerResponse> actualPostRequestResult = reportServiceAdapter.postRequest(body, new HashMap<>());

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualPostRequestResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualPostRequestResult.getBody()).isSuccess());
        assertTrue(actualPostRequestResult.hasBody());
        assertTrue(actualPostRequestResult.getHeaders().isEmpty());
    }

    @Test
    void testPostRequest3() throws RunnerException, URISyntaxException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        MailAuditLogRequest body = mock(MailAuditLogRequest.class);

        // Act
        ResponseEntity<IRunnerResponse> actualPostRequestResult = reportServiceAdapter.postRequest(body, new HashMap<>());

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualPostRequestResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualPostRequestResult.getBody()).isSuccess());
        assertTrue(actualPostRequestResult.hasBody());
        assertTrue(actualPostRequestResult.getHeaders().isEmpty());
    }

    @Test
    void testPostRequest4() throws RunnerException, URISyntaxException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        MailAuditLogRequest body = new MailAuditLogRequest();

        HashMap<String, String> parameters = new HashMap<>();
        parameters.put("Calling Report Service uri {} : with request: {}",
                "Calling Report Service uri {} : with request: {}");

        // Act
        ResponseEntity<IRunnerResponse> actualPostRequestResult = reportServiceAdapter.postRequest(body, parameters);

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualPostRequestResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualPostRequestResult.getBody()).isSuccess());
        assertTrue(actualPostRequestResult.hasBody());
        assertTrue(actualPostRequestResult.getHeaders().isEmpty());
    }

    @Test
    void testPostRequest5() throws RunnerException, URISyntaxException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenReturn(new ResponseEntity<>(HttpStatus.CONTINUE));
        MailAuditLogRequest body = new MailAuditLogRequest();

        HashMap<String, String> parameters = new HashMap<>();
        parameters.put("Retrieve CRP: with response: {}", "Retrieve CRP: with response: {}");
        parameters.put("Calling Report Service uri {} : with request: {}",
                "Calling Report Service uri {} : with request: {}");

        // Act
        ResponseEntity<IRunnerResponse> actualPostRequestResult = reportServiceAdapter.postRequest(body, parameters);

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getError());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getData());
        assertNull(((DependentServiceResponse) actualPostRequestResult.getBody()).getRequestId());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getNumberOfRecords());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageNo());
        assertEquals(0L, ((DependentServiceResponse) actualPostRequestResult.getBody()).getPageSize());
        assertEquals(HttpStatus.OK, actualPostRequestResult.getStatusCode());
        assertTrue(((DependentServiceResponse) actualPostRequestResult.getBody()).isSuccess());
        assertTrue(actualPostRequestResult.hasBody());
        assertTrue(actualPostRequestResult.getHeaders().isEmpty());
    }

    @Test
    void testPostRequest6() throws RunnerException, URISyntaxException, RestClientException {
        // Arrange
        when(restTemplate.exchange(Mockito.<RequestEntity<Object>>any(), Mockito.<Class<Object>>any()))
                .thenThrow(new HttpClientErrorException(HttpStatus.CONTINUE));
        MailAuditLogRequest body = new MailAuditLogRequest();

        // Act
        ResponseEntity<IRunnerResponse> actualPostRequestResult = reportServiceAdapter.postRequest(body, new HashMap<>());

        // Assert
        verify(restTemplate).exchange(isA(RequestEntity.class), isA(Class.class));
        ApiError error = ((RunnerResponse<Object>) actualPostRequestResult.getBody()).getError();
        assertEquals("100 CONTINUE", error.getMessage());
        assertNull(((RunnerResponse<Object>) actualPostRequestResult.getBody()).getData());
        assertNull(((RunnerResponse<Object>) actualPostRequestResult.getBody()).getRequestId());
        assertNull(error.getErrors());
        assertEquals(0, ((RunnerResponse<Object>) actualPostRequestResult.getBody()).getPageNo());
        assertEquals(0L, ((RunnerResponse<Object>) actualPostRequestResult.getBody()).getCount());
        assertEquals(HttpStatus.BAD_REQUEST, error.getStatus());
        assertEquals(HttpStatus.BAD_REQUEST, actualPostRequestResult.getStatusCode());
        assertFalse(((RunnerResponse<Object>) actualPostRequestResult.getBody()).isSuccess());
        assertTrue(actualPostRequestResult.hasBody());
        assertTrue(actualPostRequestResult.getHeaders().isEmpty());
    }
}
