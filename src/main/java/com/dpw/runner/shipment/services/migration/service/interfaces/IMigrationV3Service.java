package com.dpw.runner.shipment.services.migration.service.interfaces;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;

import java.util.Map;

public interface IMigrationV3Service {
    Map<String, Integer> migrateV2ToV3(ListCommonRequest consoleRequest, ListCommonRequest shipmentRequest);
    Map<String, Integer> migrateV3ToV2(ListCommonRequest consoleRequest, ListCommonRequest shipmentRequest);
}
