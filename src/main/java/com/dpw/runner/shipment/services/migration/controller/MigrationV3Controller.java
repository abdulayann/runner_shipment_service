package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.migration.dtos.ConsolidationMigrationRequest;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/migration/consolidation")
public class MigrationV3Controller {
    @Autowired
    private IMigrationV3Service migrationV3Service;

    @RequestMapping("/v2/v3")
    public Map<String, Integer> migrationFromV2ToV3(@RequestBody ConsolidationMigrationRequest request) {
        return migrationV3Service.migrateV2ToV3(request.getConsolidation(), request.getShipment());
    }

    @RequestMapping("/v3/v2")
    public Map<String, Integer> migrationFromV3ToV2(@RequestBody ConsolidationMigrationRequest request) {
        return migrationV3Service.migrateV3ToV2(request.getConsolidation(), request.getShipment());
    }
}
