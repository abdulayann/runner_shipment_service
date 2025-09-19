package com.dpw.runner.shipment.services.authservice;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.PostConstruct;
import java.util.Set;


@Service
public class ApiKeyService {

    @Value("${ewl.bridge.api.key:default-key}")
    private String xApiKey;

    private Set<String> validApiKeys;

    public ApiKeyService(Set<String> validKeys) {
        this.validApiKeys = validKeys;
    }

    @PostConstruct
    public void init() {
        validApiKeys = Set.of(xApiKey);
    }

    public boolean isValidApiKey(String apiKey) {
        if (apiKey == null || apiKey.trim().isEmpty()) {
            return false;
        }
        return validApiKeys.contains(apiKey);
    }
}