package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;

@Component("customKeyGenerator")
public class CustomKeyGenerator implements KeyGenerator {

    @Value("${env.name}")
    public String ENV_NAME;

    @Override
    public Object generate(Object target, Method method, Object... params) {
        StringBuilder keyBuilder = cacheBaseKey();

        switch (method.getName()) {

            case CacheConstants.GET_USER_BY_TOKEN:
                keyBuilder.append(CacheConstants.USER_DEFINITION)
                        .append(params[0].toString());
                break;

            default:
                keyBuilder.append(method.getName())
                        .append("_");
                if (params != null && params.length > 0) {
                    for (Object param : params) {
                        keyBuilder.append("_").append(param.toString());
                    }
                }
        }

        return keyBuilder.toString();
    }

    public StringBuilder cacheBaseKey() {
        return new StringBuilder(ENV_NAME).append("_");
    }

    public StringBuilder customCacheKeyForMasterData(String type, String value) {
        return new StringBuilder(ENV_NAME)
                .append("_")
                .append(type)
                .append("_")
                .append(UserContext.getUser().getTenantId())
                .append("_")
                .append(value);
    }

}

