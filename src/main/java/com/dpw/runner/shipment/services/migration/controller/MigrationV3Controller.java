package com.dpw.runner.shipment.services.migration.controller;


import com.dpw.runner.shipment.services.commons.constants.ApiConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.migration.dtos.ConsolidationMigrationRequest;
import com.dpw.runner.shipment.services.migration.service.interfaces.IMigrationV3Service;
import com.dpw.runner.shipment.services.service.impl.ApiKeyAuthenticationService;
import lombok.Generated;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = "/migration/consolidation")
@Generated
public class MigrationV3Controller {
    @Autowired
    private IMigrationV3Service migrationV3Service;
    @Autowired
    private ApiKeyAuthenticationService authenticationService;

    @PostMapping(value = "/v2/v3")
    public ResponseEntity<IRunnerResponse> migrationFromV2ToV3(@RequestBody ConsolidationMigrationRequest request, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) throws RunnerException {
        log.info("Received migration request from V2 to V3 for tenantId: {}", request.getTenantId());
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        log.debug("Authentication successful for X-API-KEY : {}", xApiKey);
        return migrationV3Service.migrateV2Tov3Async(request.getTenantId(), request.getConsolId(), request.getBookingId(), request.getCount(), request.getWeightDecimal(), request.getVolumeDecimal());
    }

    @PostMapping(value = "/v3/v2")
    public ResponseEntity<IRunnerResponse> migrationFromV3ToV2(@RequestBody ConsolidationMigrationRequest request, @RequestHeader(value = ApiConstants.X_API_KEY, required = false) String xApiKey) throws RunnerException {
        log.info("Received migration request from V3 to V2 for tenantId: {}", request.getTenantId());
        authenticationService.authenticate(Constants.MIGRATION_API, xApiKey);
        log.debug("Authentication successful for X-API-KEY: {}", xApiKey);
        return migrationV3Service.migrateV3ToV2Async(request.getTenantId(), request.getBookingId(), request.getCount());
    }

}
