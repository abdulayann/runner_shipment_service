package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.utils.Generated;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.interceptor.KeyGenerator;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Objects;

@Component("customKeyGenerator")
@Generated
@Slf4j
public class CustomKeyGenerator implements KeyGenerator {

    @Value("${env.name}")
    public String ENV_NAME;

    @Override
    public Object generate(Object target, Method method, Object... params) {
        StringBuilder keyBuilder = cacheBaseKey();

        switch (method.getName()) {

            case CacheConstants.GET_USER_BY_TOKEN:
                keyBuilder.append(CacheConstants.USER_DEFINITION)
                        .append(StringUtility.convertToString(params[0]));
                break;

            case CacheConstants.GET_TENANT_SETTINGS:
                keyBuilder.append(CacheConstants.TENANT_SETTINGS)
                        .append(StringUtility.convertToString(params[0]));
                break;

            case CacheConstants.GET_SHIPMENT_SETTINGS:
                keyBuilder.append(CacheConstants.SHIPMENT_SETTINGS)
                        .append(StringUtility.convertToString(params[0]));
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
        if (Objects.isNull(UserContext.getUser()))
            log.info("{} | User is Null", LoggerHelper.getRequestIdFromMDC());
        return new StringBuilder(ENV_NAME)
                .append("_")
                .append(type)
                .append("_")
                .append(UserContext.getUser().getTenantId())
                .append("_")
                .append(value);
    }

}

