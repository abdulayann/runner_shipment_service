package com.dpw.runner.shipment.services.adapters.config;
//
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.jupiter.api.parallel.Execution;
//import org.junit.jupiter.api.parallel.ExecutionMode;
//import org.mockito.InjectMocks;
//import org.mockito.Mock;
//import org.mockito.MockitoAnnotations;
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.springframework.http.HttpHeaders;
//import org.springframework.http.MediaType;
//import org.springframework.http.client.ClientHttpRequestExecution;
//
//import java.io.IOException;
//
//import static org.junit.jupiter.api.Assertions.assertEquals;
//
//@ExtendWith(MockitoExtension.class)
//@Execution(ExecutionMode.CONCURRENT)
//public class FusionConfigTest {
//
//    @Mock
//    private ClientHttpRequestExecution execution;
//
//    @InjectMocks
//    private FusionConfig fusionConfig;
//
//    @BeforeEach
//    void setUp() {
//        MockitoAnnotations.openMocks(this);
//    }
//
//    @Test
//    public void testRestTemplateForCRP() throws IOException {
//        HttpHeaders headers = new HttpHeaders();
//        MediaType mediaType = new MediaType(MediaType.APPLICATION_JSON);
//        headers.setContentType(mediaType);
//        fusionConfig.restTemplateForCreditCheckP100();
//        assertEquals("application/json", headers.getContentType().toString());
//    }
//
//}

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.ClientHttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.test.context.TestPropertySource;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
@TestPropertySource(properties = {
        "fusion.P100.username=testUsername",
        "fusion.P100.password=testPassword"
})
public class FusionConfigTest {

    @Value("${fusion.P100.username}")
    private String usernameP100;

    @Value("${fusion.P100.password}")
    private String passwordP100;

    @InjectMocks
    private FusionConfig fusionConfig;

    @BeforeEach
    public void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    public void testRestTemplateForCreditCheckP100() throws IOException {
        RestTemplate restTemplate = fusionConfig.restTemplateForCreditCheckP100();
        List<ClientHttpRequestInterceptor> interceptors = restTemplate.getInterceptors();

        assertFalse(interceptors.isEmpty(), "Interceptors should not be empty");
        ClientHttpRequestInterceptor interceptor = interceptors.get(0);

        ClientHttpRequestExecution execution = mock(ClientHttpRequestExecution.class);
        ClientHttpResponse response = mock(ClientHttpResponse.class);
        when(execution.execute(any(), any())).thenReturn(response);

        HttpHeaders headers = new HttpHeaders();
        ClientHttpRequest request = mock(ClientHttpRequest.class);
        when(request.getHeaders()).thenReturn(headers);

        interceptor.intercept(request, new byte[0], execution);

        assertEquals(MediaType.APPLICATION_JSON, headers.getContentType(), "ContentType should be application/json");
        assertTrue(headers.containsKey(HttpHeaders.AUTHORIZATION), "Authorization header should be set");
        String authHeader = headers.getFirst(HttpHeaders.AUTHORIZATION);
        assertNotNull(authHeader, "Authorization header should not be null");
        assertTrue(authHeader.startsWith("Basic "), "Authorization header should start with Basic");
    }
}
