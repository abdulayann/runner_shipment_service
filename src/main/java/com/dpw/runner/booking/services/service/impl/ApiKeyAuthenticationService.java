package com.dpw.runner.booking.services.service.impl;

import com.dpw.runner.booking.services.commons.constants.CacheConstants;
import com.dpw.runner.booking.services.exception.exceptions.UnAuthorizedException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
public class ApiKeyAuthenticationService {

    @Value("${x-api-key.v1-cache}")
    private String cacheApiKey;

    public void authenticate(String module, String apiKey) {

        switch (module) {
            case CacheConstants.CACHE:
                if (!Objects.equals(cacheApiKey, apiKey))
                    throw new UnAuthorizedException("Invalid API Key");

                break;
            default:

        }


    }

}
