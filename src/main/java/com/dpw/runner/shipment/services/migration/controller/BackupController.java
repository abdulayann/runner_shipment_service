package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.migration.strategy.interfaces.BackupService;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/backup")
@Slf4j
public class BackupController {

    @Autowired
    private BackupService backupService;

    @PostMapping
    public ResponseEntity<String> backupTenantData(@RequestParam Integer tenantId) {
        backupService.backupTenantData(tenantId);
        return ResponseEntity.ok("Backup completed for tenant: " + tenantId);
    }
}
