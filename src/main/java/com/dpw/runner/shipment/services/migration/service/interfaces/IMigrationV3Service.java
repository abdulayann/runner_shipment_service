package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;

import java.util.Map;

public interface IMigrationV3Service {
    Map<String, Integer> migrateV2ToV3(Integer tenantId, Long consolId);
    Map<String, Integer> migrateV3ToV2(Integer tenantId);

    Map<String, Integer> bookingV2ToV3Migration(Integer tenantId, Long bookingId);

    Map<String, Integer> bookingV3ToV2Migration(Integer tenantId, Long bookingId);
}
