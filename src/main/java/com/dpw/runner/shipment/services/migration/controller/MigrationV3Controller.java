package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.migration.dtos.ConsolidationMigrationRequest;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

@RestController
@RequestMapping(value = "/migration/consolidation")
public class MigrationV3Controller {
    @Autowired
    private IMigrationV3Service migrationV3Service;
    @Autowired
    private ApiKeyAuthenticationService authenticationService;

    @PostMapping("/v2/v3")
    public Map<String, Integer> migrationFromV2ToV3(@RequestBody ConsolidationMigrationRequest request, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        return migrationV3Service.migrateV2ToV3(request.getTenantId(), request.getConsolId());
    }

    @RequestMapping("/v3/v2")
    public Map<String, Integer> migrationFromV3ToV2(@RequestBody ConsolidationMigrationRequest request, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        return migrationV3Service.migrateV3ToV2(request.getTenantId());
    }
}
