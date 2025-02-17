package com.dpw.runner.shipment.services.adapters.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import java.util.Collections;

@Configuration
public class NPMConfig {

    @Value("${NPM.xApikeyV2}")
    private String xApikeyV2;

    @Value("${npm.exchange.rate.api.key}")
    private String xApiKey;
    @Value("${NPM.multiLang.xApiKey}")
    private String multiLangXApiKey;

    @Bean
    public RestTemplate restTemplateForNPM() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set("x-api-key-v2", xApikeyV2);
            return execution.execute(request, body);
        });
        return restTemplate;
    }

    @Bean
    public RestTemplate restTemplateForExchangeRates() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set(ApiConstants.X_API_KEY, xApiKey);
            return execution.execute(request, body);
        });
        return restTemplate;
    }

    @Bean
    public RestTemplate restTemplateForNpmService() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set(ApiConstants.X_API_KEY, xApiKey);
            headers.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));
            headers.add("Authorization", RequestAuthContext.getAuthToken());
            return execution.execute(request, body);
        });
        return restTemplate;
    }

    @Bean
    public RestTemplate restTemplateForNpmMultiLangChargeCode() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set(ApiConstants.X_API_KEY, multiLangXApiKey);
            return execution.execute(request, body);
        });
        return restTemplate;
    }
}
