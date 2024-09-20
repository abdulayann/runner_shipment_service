package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.dto.request.CacheRequest;
import com.dpw.runner.shipment.services.exception.exceptions.CacheEvictionException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.utils.StringUtility;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.Objects;

@Component
@Slf4j
public class CacheEvictionService {

    @Autowired
    CacheManager cacheManager;
    @Autowired
    CustomKeyGenerator keyGenerator;
    private String baseKey;
    @Autowired
    private JsonHelper jsonHelper;

    @PostConstruct
    public void setKey() {
        this.baseKey = StringUtility.convertToString(keyGenerator.cacheBaseKey());
    }

    public void clearAllCache() {
        try {
            cacheManager.getCache(CacheConstants.CACHE_KEY).clear();
            cacheManager.getCache(CacheConstants.CACHE_KEY_USER).clear();
            cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA).clear();
        } catch (Exception e) {
            log.error("Error during evicting cache {}", e);
            throw new CacheEvictionException(e.getMessage());
        }
    }

    public void clearCacheByName(CommonRequestModel commonRequestModel) {
        try {
            CacheRequest request = (CacheRequest) commonRequestModel.getData();
            cacheManager.getCache(CacheConstants.CACHE_KEY_USER).evictIfPresent(baseKey + request.getKey());
        } catch (Exception e) {
            log.error("Error during evicting cache {}", e);
            throw new CacheEvictionException(e.getMessage());
        }
    }

    public void clearCacheByName(String prefixKey, String suffixKey) {
        try {
            log.info("clearCacheByName for key {}::{}", prefixKey, baseKey + suffixKey);
            var cache = cacheManager.getCache(prefixKey);
            if (!Objects.isNull(cache))  cache.evictIfPresent(baseKey + suffixKey);
        } catch (Exception e) {
            log.error("Error during evicting cache with key: {}::{} with exception: {}",prefixKey, baseKey + suffixKey, e.getMessage());
        }
    }
    
}
