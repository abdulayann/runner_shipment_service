package com.dpw.runner.shipment.services.adapters.config;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

@Configuration
@Getter
public class BridgeServiceConfig {


    @Value("${bridgeService.tenantCode}")
    private String tenantCode;
    @Value("${bridgeService.userName}")
    private String userName;
    @Value("${bridgeService.password}")
    private String password;


    @Value("${bridgeService.tactIntegrationRequestCode}")
    private String tactIntegrationRequestCode;

    @Value("${bridgeService.baseUrl}")
    private String baseUrl;
    @Value("${bridgeService.authLoginUrl}")
    private String authLoginUrl;
    @Value("${bridgeService.requestUrl}")
    private String requestUrl;

    private final String xClientType = "STOS";


    @Bean
    RestTemplate restTemplateForBridgeService() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set(ApiConstants.X_CLIENT_TYPE, xClientType);
            return execution.execute(request, body);
        });
        return restTemplate;
    }

}
