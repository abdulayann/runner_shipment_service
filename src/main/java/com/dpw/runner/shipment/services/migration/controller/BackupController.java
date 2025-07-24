package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.migration.strategy.interfaces.TenantDataBackupService;
import lombok.extern.slf4j.Slf4j;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
/**
    V2 backup controller at 0th day.
 */
@RestController
@RequestMapping("/api/backup")
@Slf4j
public class BackupController {

    @Autowired
    private TenantDataBackupService backupService;

    @PostMapping
    public ResponseEntity<Void> backupTenantData(@RequestParam Integer tenantId) {
        try {
            backupService.backupTenantData(tenantId);
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            log.error("Backup failed for tenant: {}", tenantId, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }

}
