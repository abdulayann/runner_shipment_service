package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.DaoConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.CacheRequest;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import com.dpw.runner.shipment.services.service.impl.CacheEvictionService;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@Slf4j
@RequestMapping(CacheConstants.CACHE_API_HANDLE)
public class CacheEvictionController {

    private final CacheEvictionService cacheEviction;
    private final ApiKeyAuthenticationService authenticationService;


    @Autowired
    public CacheEvictionController(CacheEvictionService cacheEviction, ApiKeyAuthenticationService authenticationService) {
        this.cacheEviction = cacheEviction;
        this.authenticationService = authenticationService;
    }

    @GetMapping(CacheConstants.EVICT_ALL_CACHE)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> evictAllCache(@RequestHeader(ApiConstants.X_API_KEY) String xApiKey) {
        String responseMsg;
        try {
            log.info("Request received for EVICT_ALL_CACHE");
            authenticationService.authenticate(CacheConstants.CACHE, xApiKey);
            cacheEviction.clearAllCache();
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error("Request failed for EVICT_ALL_CACHE with exception: {}", responseMsg);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }

    @PostMapping(CacheConstants.EVICT_CACHE_BY_KEY)
    @ExcludeTimeZone
    public ResponseEntity<IRunnerResponse> evictCache(@RequestBody CacheRequest request, @RequestHeader(ApiConstants.X_API_KEY) String xApiKey) {
        String responseMsg;
        try {
            log.info("Request received for EVICT_CACHE_BY_KEY with key: {}", request.getKey());
            authenticationService.authenticate(CacheConstants.CACHE, xApiKey);
            cacheEviction.clearCacheByName(CommonRequestModel.builder().data(request).build());
            return ResponseHelper.buildSuccessResponse();
        } catch (Exception e) {
            responseMsg = e.getMessage() != null ? e.getMessage()
                    : DaoConstants.DAO_GENERIC_CREATE_EXCEPTION_MSG;
            log.error("Request failed for EVICT_CACHE_BY_KEY with exception: {}", responseMsg);
        }
        return ResponseHelper.buildFailedResponse(responseMsg);
    }
}
