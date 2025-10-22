package com.dpw.runner.shipment.services.adapters.config;

import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.mock.http.client.MockClientHttpRequest;
import org.springframework.mock.http.client.MockClientHttpResponse;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class V1ConfigTest {

    private V1Config v1Config;

    @BeforeEach
    void setUp() {
        v1Config = new V1Config();
    }

    @Test
    void testRestTemplateForV1_ShouldReturnNonNullRestTemplate() {
        // When
        RestTemplate restTemplate = v1Config.restTemplateForV1();

        // Then
        assertThat(restTemplate).isNotNull();
    }

    @Test
    void testRestTemplateForV1_ShouldHaveInterceptors() {
        // When
        RestTemplate restTemplate = v1Config.restTemplateForV1();

        // Then
        assertThat(restTemplate.getInterceptors()).hasSize(1);
    }

    @Test
    void testRestTemplateForV1_InterceptorShouldAddSourceServiceTypeHeader() throws IOException {
        // Given
        RestTemplate restTemplate = v1Config.restTemplateForV1();
        List<ClientHttpRequestInterceptor> interceptors = restTemplate.getInterceptors();

        MockClientHttpRequest request = new MockClientHttpRequest();
        ClientHttpRequestExecution execution = mock(ClientHttpRequestExecution.class);
        ClientHttpResponse mockResponse = new MockClientHttpResponse(new byte[0], 200);
        when(execution.execute(any(), any())).thenReturn(mockResponse);

        // When
        interceptors.get(0).intercept(request, new byte[0], execution);

        // Then
        HttpHeaders headers = request.getHeaders();
        assertThat(headers.get("SourceServiceType")).containsExactly("Shipment");
    }

    @Test
    void testRestTemplateForV1_InterceptorShouldAddXRequestedWithHeader() throws IOException {
        // Given
        RestTemplate restTemplate = v1Config.restTemplateForV1();
        List<ClientHttpRequestInterceptor> interceptors = restTemplate.getInterceptors();

        MockClientHttpRequest request = new MockClientHttpRequest();
        ClientHttpRequestExecution execution = mock(ClientHttpRequestExecution.class);
        ClientHttpResponse mockResponse = new MockClientHttpResponse(new byte[0], 200);
        when(execution.execute(any(), any())).thenReturn(mockResponse);

        // When
        interceptors.get(0).intercept(request, new byte[0], execution);

        // Then
        HttpHeaders headers = request.getHeaders();
        assertThat(headers.get("X-Requested-With")).containsExactly("XMLHttpRequest");
    }

    @Test
    void testRestTemplateForV1_InterceptorShouldExecuteRequest() throws IOException {
        // Given
        RestTemplate restTemplate = v1Config.restTemplateForV1();
        List<ClientHttpRequestInterceptor> interceptors = restTemplate.getInterceptors();

        MockClientHttpRequest request = new MockClientHttpRequest();
        ClientHttpRequestExecution execution = mock(ClientHttpRequestExecution.class);
        ClientHttpResponse mockResponse = new MockClientHttpResponse(new byte[0], 200);
        when(execution.execute(any(), any())).thenReturn(mockResponse);

        // When
        ClientHttpResponse response = interceptors.get(0).intercept(request, new byte[0], execution);

        // Then
        assertThat(response).isNotNull();
        assertThat(response).isEqualTo(mockResponse);
    }

    @Test
    void testRestTemplateForV1_InterceptorShouldAddBothHeaders() throws IOException {
        // Given
        RestTemplate restTemplate = v1Config.restTemplateForV1();
        List<ClientHttpRequestInterceptor> interceptors = restTemplate.getInterceptors();

        MockClientHttpRequest request = new MockClientHttpRequest();
        ClientHttpRequestExecution execution = mock(ClientHttpRequestExecution.class);
        ClientHttpResponse mockResponse = new MockClientHttpResponse(new byte[0], 200);
        when(execution.execute(any(), any())).thenReturn(mockResponse);

        // When
        interceptors.get(0).intercept(request, new byte[0], execution);

        // Then
        HttpHeaders headers = request.getHeaders();
        assertThat(headers.get("SourceServiceType")).containsExactly("Shipment");
        assertThat(headers.get("X-Requested-With")).containsExactly("XMLHttpRequest");
        assertThat(headers).hasSize(2);
    }

    @Test
    void testRestTemplateForV1_ShouldHaveMessageConverters() {
        // When
        RestTemplate restTemplate = v1Config.restTemplateForV1();

        // Then
        assertThat(restTemplate.getMessageConverters()).isNotEmpty();
    }

    @Test
    void testConfigurationAnnotation_ShouldBePresent() {
        // Then
        assertThat(v1Config.getClass().isAnnotationPresent(
                org.springframework.context.annotation.Configuration.class)).isTrue();
    }
}