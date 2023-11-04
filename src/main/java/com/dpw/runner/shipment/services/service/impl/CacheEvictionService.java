package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.CacheRequest;
import com.dpw.runner.shipment.services.exception.exceptions.CacheEvictionException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

@Component
@Slf4j
public class CacheEvictionService {

    @Autowired
    CacheManager cacheManager;
    @Autowired
    CustomKeyGenerator keyGenerator;
    private String baseKey;

    @PostConstruct
    public void setKey() {
        this.baseKey = keyGenerator.cacheBaseKey().toString();
    }

    public void clearAllCache() {
        try {
            cacheManager.getCache(CacheConstants.CACHE_KEY).clear();
        } catch (Exception e) {
            log.error("Error during evicting cache {}", e);
            throw new CacheEvictionException(e.getMessage());
        }
    }

    public void clearCacheByName(CommonRequestModel commonRequestModel) {
        try {
            CacheRequest request = (CacheRequest) commonRequestModel.getData();
            cacheManager.getCache(CacheConstants.CACHE_KEY).evictIfPresent(baseKey + request.getKey());
        } catch (Exception e) {
            log.error("Error during evicting cache {}", e);
            throw new CacheEvictionException(e.getMessage());
        }
    }

}
