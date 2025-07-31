package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataRestoreService;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/restore")
@Slf4j
public class RestoreController {

    @Autowired
    private ApiKeyAuthenticationService authenticationService;
    @Autowired
    private TenantDataRestoreService restoreService;

    @PostMapping
    public ResponseEntity<String> backupTenantData(@RequestParam Integer tenantId, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        restoreService.restoreTenantData(tenantId);
        return ResponseEntity.ok("restore completed for tenant: " + tenantId);
    }
}
