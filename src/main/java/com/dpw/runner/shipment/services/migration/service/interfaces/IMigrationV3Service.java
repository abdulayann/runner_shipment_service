package com.dpw.runner.shipment.services.migration.service.interfaces;


import java.util.Map;

public interface IMigrationV3Service {
    Map<String, Integer> migrateV2ToV3(Integer tenantId, Long consolId, Long bookingId);
    Map<String, Integer> migrateV3ToV2(Integer tenantId, Long bookingId);
}
