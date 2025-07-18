package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.exception.exceptions.UnAuthorizedException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
public class ApiKeyAuthenticationService {

    @Value("${x-api-key.v1-cache}")
    private String cacheApiKey;

    @Value("${x-api-key.v2-tracking-push}")
    private String trackingPushApiKey;

    @Value("${x-api-key.migration-3.0}")
    private String migrationApiKey;

    public void authenticate(String module, String apiKey) {
        String expectedApiKey = switch (module) {
            case CacheConstants.CACHE -> cacheApiKey;
            case Constants.TRACKING_PUSH_API -> trackingPushApiKey;
            case Constants.MIGRATION_API -> migrationApiKey;
            default -> null;
        };

        if (!Objects.equals(expectedApiKey, apiKey)) {
            throw new UnAuthorizedException("Invalid API Key");
        }
    }

}
