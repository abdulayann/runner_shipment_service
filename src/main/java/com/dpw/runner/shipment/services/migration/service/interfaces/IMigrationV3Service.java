package com.dpw.runner.shipment.services.migration.service.interfaces;


import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import org.springframework.http.ResponseEntity;

import java.util.Map;

public interface IMigrationV3Service {
    ResponseEntity<IRunnerResponse> migrateV2Tov3Async(Integer tenantId, Long consolId, Long bookingId, Integer count);
    Map<String, Integer> migrateV2ToV3(Integer tenantId, Long consolId, Long bookingId, Integer count);
    Map<String, Integer> migrateV3ToV2(Integer tenantId, Long bookingId, Integer count);
    ResponseEntity<IRunnerResponse> migrateV3ToV2Async(Integer tenantId, Long bookingId, Integer count);
}
