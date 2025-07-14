package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.migration.entity.enums.EntityType;
import com.dpw.runner.shipment.services.migration.strategy.BackupStrategyFactory;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/backup")
@Slf4j
public class BackupController {

    @Autowired
    private BackupStrategyFactory backupStrategyFactory;

    @PostMapping()
    public ResponseEntity<String> backup(@RequestParam Integer tenantId) {

        backupStrategyFactory.getStrategy(EntityType.SHIPMENT).backup(tenantId);
        return ResponseEntity.ok("Backup done for shipment");
    }

    @DeleteMapping("/{entityType}")
    public ResponseEntity<String> deleteBackup(@PathVariable EntityType entityType, @RequestParam Integer tenantId) {
        backupStrategyFactory.getStrategy(entityType).delete(tenantId);
        return ResponseEntity.ok("Backup deleted for tenantId: " + tenantId);
    }
}
