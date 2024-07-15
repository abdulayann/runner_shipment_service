package com.dpw.runner.shipment.services.adapters.config;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class BridgeServiceConfigTest {

    @Mock
    private ClientHttpRequestExecution execution;

    @InjectMocks
    private BridgeServiceConfig bridgeServiceConfig;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testRestTemplateForBridgeService() throws IOException {
        RestTemplate restTemplate = bridgeServiceConfig.restTemplateForBridgeService();
        List<ClientHttpRequestInterceptor> interceptors = restTemplate.getInterceptors();

        assertEquals(1, interceptors.size(), "There should be one interceptor");

        ClientHttpRequestInterceptor interceptor = interceptors.get(0);

        HttpRequest requestWrapper = mock(HttpRequest.class);
        HttpHeaders headers = new HttpHeaders();
        when(requestWrapper.getHeaders()).thenReturn(headers);

        ClientHttpResponse response = mock(ClientHttpResponse.class);
        when(execution.execute(any(), any(byte[].class))).thenReturn(response);

        interceptor.intercept(requestWrapper, new byte[0], execution);
        assertEquals("application/json", headers.getContentType().toString());
    }
}
