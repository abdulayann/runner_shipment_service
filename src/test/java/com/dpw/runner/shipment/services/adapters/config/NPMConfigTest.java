package com.dpw.runner.shipment.services.adapters.config;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {NPMConfig.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
class NPMConfigTest {
    @Autowired
    private NPMConfig npmConfig;
    @MockBean
    private ClientHttpRequestExecution execution;

    @Test
    void testRestTemplateForNPM() throws IOException {
        RestTemplate restTemplate = npmConfig.restTemplateForNPM();
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

    @Test
    void testRestTemplateForExchangeRates() throws IOException {
        RestTemplate restTemplate = npmConfig.restTemplateForExchangeRates();
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

    @Test
    void testRestTemplateForNpmService() throws IOException {
        RestTemplate restTemplate = npmConfig.restTemplateForNpmService();
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

    @Test
    void testRestTemplateForNpmMultiLangChargeCode() throws IOException {
        RestTemplate restTemplate = npmConfig.restTemplateForNpmMultiLangChargeCode();
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
