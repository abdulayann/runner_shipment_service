package com.dpw.runner.shipment.services.migration;

import com.dpw.runner.shipment.services.migration.map.console.v2ToV3.ConsolidationV2ToV3Migration;

public class V2toV3Migration {
    public static void main(String[] args) {
        ConsolidationV2ToV3Migration migration = new ConsolidationV2ToV3Migration();
        migration.migrate(null, null);
    }
}
