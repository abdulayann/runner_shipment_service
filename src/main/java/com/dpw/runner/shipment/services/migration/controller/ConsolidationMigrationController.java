package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.migration.dtos.ConsolidationMigrationRequest;
import com.dpw.runner.shipment.services.migration.map.console.v2ToV3.ConsolidationV2ToV3Migration;
import com.dpw.runner.shipment.services.migration.map.console.v3ToV2.ConsolidationV3ToV2Migration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping("/migration/consolidation")
public class ConsolidationMigrationController {

    @Autowired
    private ConsolidationV2ToV3Migration consolidationV2ToV3Migration;

    @Autowired
    private ConsolidationV3ToV2Migration consolidationV3ToV2Migration;

    @RequestMapping("/v2/v3")
    public Map<String, Integer> migrationFromV2ToV3(@RequestBody ConsolidationMigrationRequest request) {
        return consolidationV2ToV3Migration.migrate(request.getConsolidation(), request.getShipment());
    }

    @RequestMapping("/v3/v3")
    public Map<String, Integer> migrationFromV3ToV2(@RequestBody ConsolidationMigrationRequest request) {
        return consolidationV3ToV2Migration.migrate(request.getConsolidation(), request.getShipment());
    }
}
