package com.dpw.runner.shipment.services.migration.controller;

import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.migration.rollback.EntityLevelRollbackService;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Slf4j
@RestController
@RequestMapping
public class RollBackController {

    @Autowired
    private ApiKeyAuthenticationService authenticationService;

    @Autowired
    private EntityLevelRollbackService entityLevelRollbackService;

    @GetMapping("/rollback/listSchema")
    public List<?> migrationFromV2ToV3(@RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        return entityLevelRollbackService.listSchemas();
    }

    @GetMapping("/rollback/execute")
    public String rollback(@RequestParam String schema, @RequestParam String tenantId, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        entityLevelRollbackService.executeSqlFromFile(tenantId, schema);
        return "Done";
    }

    @GetMapping("/rollback/createBackUpSchema")
    public String backup(@RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        return String.format("New Schema has been created %s", entityLevelRollbackService.backupEntity());
    }
}
