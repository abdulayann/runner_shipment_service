package com.dpw.runner.shipment.services.adapters.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.utils.ContextUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

@Configuration
public class OrderManagementConfig {

    @Autowired
    private ContextUtility contextUtility;

    @Bean
    public RestTemplate restTemplateForOrderManagement() {
        RestTemplate restTemplate = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
        restTemplate.getInterceptors().add((request, body, execution) -> {
            HttpHeaders headers = request.getHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set("x-tenant-id", contextUtility.tenantContext.getCurrentTenant().toString());
            headers.add("Authorization", contextUtility.requestAuthContext.getAuthToken());
            return execution.execute(request, body);
        });
        return restTemplate;
    }
}
