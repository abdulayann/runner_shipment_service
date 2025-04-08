package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class TenantSettingsService {

    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private IV1Service v1Service;

    @Cacheable(cacheNames = CacheConstants.CACHE_KEY_USER, keyGenerator = "customKeyGenerator")
    public V1TenantSettingsResponse getV1TenantSettings(Integer tenantId)
    {
        log.info("Request: {} getV1TenantSettings --- for tenant: {}", LoggerHelper.getRequestIdFromMDC(), tenantId);
        V1RetrieveResponse dependentServiceResponse = v1Service.retrieveTenantSettings();
        if(dependentServiceResponse != null)
        {
            return modelMapper.map(dependentServiceResponse.getEntity(), V1TenantSettingsResponse.class);
        }
        return null;
    }
}
