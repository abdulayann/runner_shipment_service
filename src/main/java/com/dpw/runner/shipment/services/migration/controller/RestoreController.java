package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataRestoreService;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/restore")
@Slf4j
@Generated
public class RestoreController {

    @Autowired
    private ApiKeyAuthenticationService authenticationService;
    @Autowired
    private TenantDataRestoreService restoreService;

    @PostMapping
    public ResponseEntity<String> backupTenantData(@RequestParam Integer tenantId, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey, @RequestParam Integer count) throws RunnerException {
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        return restoreService.restoreTenantDataAsync(tenantId, count);
    }
}
