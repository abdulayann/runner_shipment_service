package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.migration.entity.enums.EntityType;
import com.dpw.runner.shipment.services.migration.strategy.BackupStrategyFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/restore")
public class RestoreController {

    @Autowired
    private BackupStrategyFactory backupStrategyFactory;

    @PostMapping("/{entityType}")
    public ResponseEntity<String> backup(@PathVariable EntityType entityType, @RequestParam Integer tenantId) {
        backupStrategyFactory.getStrategy(entityType).restore(tenantId);
        return ResponseEntity.ok("Backup done for " + entityType);
    }

}
