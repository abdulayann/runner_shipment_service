package com.dpw.runner.shipment.services.adapters.config;

import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager;
import org.apache.hc.core5.util.Timeout;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import java.util.concurrent.TimeUnit;

@Configuration
public class V1Config {


    @Bean
    public RestTemplate restTemplateForV1() {
        PoolingHttpClientConnectionManager connectionManager = new PoolingHttpClientConnectionManager();
        connectionManager.setMaxTotal(20); // Set the maximum total connections
        connectionManager.setDefaultMaxPerRoute(10);

        // Configure timeouts using RequestConfig
        RequestConfig requestConfig = RequestConfig.custom()
                .setConnectionRequestTimeout(60, TimeUnit.SECONDS)
                .setResponseTimeout(Timeout.ofSeconds(60))
                .build();

        CloseableHttpClient httpClient = HttpClientBuilder.create()
                .setConnectionManager(connectionManager)
                .setDefaultRequestConfig(requestConfig)
                .build();
        HttpComponentsClientHttpRequestFactory requestFactory = new HttpComponentsClientHttpRequestFactory(httpClient);
        requestFactory.setConnectTimeout(60 * 1000);

        RestTemplate restTemplate = new RestTemplate(requestFactory);
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.add("SourceServiceType", "Shipment");
            headers.add("X-Requested-With", "XMLHttpRequest");
            return execution.execute(request, body);
        });

        return restTemplate;
    }
}
