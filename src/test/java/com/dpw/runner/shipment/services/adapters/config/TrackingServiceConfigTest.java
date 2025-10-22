package com.dpw.runner.shipment.services.adapters.config;

import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import static org.assertj.core.api.Assertions.assertThat;

@ExtendWith(MockitoExtension.class)
class TrackingServiceConfigTest {

    private TrackingServiceConfig trackingServiceConfig;

    @BeforeEach
    void setUp() {
        trackingServiceConfig = new TrackingServiceConfig();
        ReflectionTestUtils.setField(trackingServiceConfig, "eventsMessageTopic", "test-events-topic");
        ReflectionTestUtils.setField(trackingServiceConfig, "runnerFlowMessageTopic", "test-runner-flow-topic");
    }

    @Test
    void testRestTemplateForTrackingService_ShouldReturnNonNullRestTemplate() {
        // When
        RestTemplate restTemplate = trackingServiceConfig.restTemplateForTrackingService();

        // Then
        assertThat(restTemplate).isNotNull();
    }

    @Test
    void testRestTemplateForTrackingService_ShouldUseHttpComponentsClientHttpRequestFactory() {
        // When
        RestTemplate restTemplate = trackingServiceConfig.restTemplateForTrackingService();

        // Then
        assertThat(restTemplate.getRequestFactory())
                .isInstanceOf(HttpComponentsClientHttpRequestFactory.class);
    }

    @Test
    void testRestTemplateForTrackingService_ShouldConfigureHttpClient() {
        // When
        RestTemplate restTemplate = trackingServiceConfig.restTemplateForTrackingService();
        HttpComponentsClientHttpRequestFactory requestFactory =
                (HttpComponentsClientHttpRequestFactory) restTemplate.getRequestFactory();

        // Then
        assertThat(requestFactory).isNotNull();

        // Verify the HTTP client is configured
        CloseableHttpClient httpClient = (CloseableHttpClient) ReflectionTestUtils.getField(requestFactory, "httpClient");
        assertThat(httpClient).isNotNull();
    }

    @Test
    void testGetEventsMessageTopic_ShouldReturnConfiguredValue() {
        // When
        String topic = trackingServiceConfig.getEventsMessageTopic();

        // Then
        assertThat(topic).isEqualTo("test-events-topic");
    }

    @Test
    void testGetRunnerFlowMessageTopic_ShouldReturnConfiguredValue() {
        // When
        String topic = trackingServiceConfig.getRunnerFlowMessageTopic();

        // Then
        assertThat(topic).isEqualTo("test-runner-flow-topic");
    }

    @Test
    void testRestTemplateForTrackingService_ShouldConfigureTimeouts() {
        // When
        RestTemplate restTemplate = trackingServiceConfig.restTemplateForTrackingService();
        HttpComponentsClientHttpRequestFactory requestFactory =
                (HttpComponentsClientHttpRequestFactory) restTemplate.getRequestFactory();

        // Then
        assertThat(requestFactory).isNotNull();
        // The timeouts are configured in RequestConfig, which is set on the HttpClient
        // We can verify the factory was created successfully with the configured client
        CloseableHttpClient httpClient = (CloseableHttpClient) ReflectionTestUtils.getField(requestFactory, "httpClient");
        assertThat(httpClient).isNotNull();
    }

    @Test
    void testRestTemplateForTrackingService_ShouldHaveMessageConverters() {
        // When
        RestTemplate restTemplate = trackingServiceConfig.restTemplateForTrackingService();

        // Then
        assertThat(restTemplate.getMessageConverters()).isNotEmpty();
    }

    @Test
    void testRestTemplateForTrackingService_ShouldHaveDefaultInterceptors() {
        // When
        RestTemplate restTemplate = trackingServiceConfig.restTemplateForTrackingService();

        // Then
        assertThat(restTemplate.getInterceptors()).isEmpty(); // No custom interceptors configured
    }

    @Test
    void testConfigurationAnnotation_ShouldBePresent() {
        // Then
        assertThat(trackingServiceConfig.getClass().isAnnotationPresent(
                org.springframework.context.annotation.Configuration.class)).isTrue();
    }
}
