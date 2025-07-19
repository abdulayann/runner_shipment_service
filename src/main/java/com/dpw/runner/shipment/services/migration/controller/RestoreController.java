package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.migration.strategy.interfaces.RestoreService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/restore")
@Slf4j
public class RestoreController {

    @Autowired
    private RestoreService restoreService;

    @PostMapping
    public ResponseEntity<String> backupTenantData(@RequestParam Integer tenantId) {
        restoreService.restoreTenantData(tenantId);
        return ResponseEntity.ok("restore completed for tenant: " + tenantId);
    }
}
