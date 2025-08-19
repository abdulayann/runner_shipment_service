package com.dpw.runner.shipment.services.migration.controller;


import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.migration.dtos.ConsolidationMigrationRequest;
import com.dpw.runner.shipment.services.migration.service.interfaces.ICustomerBookingV3MigrationService;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;
@Slf4j
@RestController
@RequestMapping(value = "/migration/consolidation")
@Generated
public class MigrationV3Controller {
    @Autowired
    private IMigrationV3Service migrationV3Service;
    @Autowired
    private ApiKeyAuthenticationService authenticationService;
    @Autowired
    private ICustomerBookingV3MigrationService customerBookingV3MigrationService;

    @PostMapping(value = "/v2/v3")
    public ResponseEntity<IRunnerResponse> migrationFromV2ToV3(@RequestBody ConsolidationMigrationRequest request, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        log.info("Received migration request from V2 to V3 for tenantId: {}", request.getTenantId());
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        log.debug("Authentication successful for X-API-KEY: {}", xApiKey);
        return migrationV3Service.migrateV2Tov3Async(request.getTenantId(), request.getConsolId(), request.getBookingId());
    }

    @PostMapping(value = "/v3/v2")
    public ResponseEntity<IRunnerResponse> migrationFromV3ToV2(@RequestBody ConsolidationMigrationRequest request, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) {
        log.info("Received migration request from V3 to V2 for tenantId: {}", request.getTenantId());
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        log.debug("Authentication successful for X-API-KEY: {}", xApiKey);
        return migrationV3Service.migrateV3ToV2Async(request.getTenantId(), request.getBookingId());
    }

    @PostMapping(value = "/booking/v2/v3")
    public Map<String, Integer> bookingMigrationFromV2ToV3(@RequestParam String id) {
//        log.info("Received migration request from V2 to V3 for tenantId: {}", request.getTenantId());
//        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
//        log.debug("Authentication successful for X-API-KEY: {}", xApiKey);
        Map<String, Integer> result = customerBookingV3MigrationService.migrateV2ToV3ForSingleBooking(Long.valueOf(id));
        log.info("Migration from V2 to V3 completed for bookingId: {}. Result: {}", id, result);
        return result;
    }

    @PostMapping(value = "/booking/v3/v2")
    public Map<String, Integer> bookingMigrationFromV3ToV2(@RequestParam String id) {
        log.info("Received migration request from V3 to V2 for bookingId: {}", id);
//        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
//        log.debug("Authentication successful for X-API-KEY: {}", xApiKey);
        Map<String, Integer> result = customerBookingV3MigrationService.migrateV3ToV2ForSingleBooking(Long.valueOf(id));
        log.info("Migration from V3 to V2 completed for bookingId: {}. Result: {}", id, result);
        return result;
    }

}
