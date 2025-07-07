package com.dpw.runner.shipment.services.migration.map.console.v3ToV2;

import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import com.dpw.runner.shipment.services.migration.map.console.AbstractConsolidationMigration;
import com.dpw.runner.shipment.services.migration.map.console.ConsoleDBMapper;
import com.dpw.runner.shipment.services.migration.map.console.v2ToV3.ConsoleV2DBMapper;
import com.dpw.runner.shipment.services.migration.map.console.v2ToV3.ConsolidationDTOV2ToV3Mapper;
import com.dpw.runner.shipment.services.migration.map.shipment.AbstractShipmentMigration;
import com.dpw.runner.shipment.services.migration.map.shipment.v3ToV2.ShipmentMigrationV3ToV2;
import org.springframework.beans.factory.annotation.Autowired;

public class ConsolidationV3ToV2Migration extends AbstractConsolidationMigration<ConsolidationDetailsV3Response, ConsolidationDetailsResponse, ShipmentDetailsV3Response, ShipmentDetailsResponse> {

    @Autowired
    private ShipmentMigrationV3ToV2 migrationV3ToV2;

    @Override
    protected ConsoleDBMapper<ConsolidationDetailsV3Response> getConsoleDBMapperFrom() {
        return new ConsoleV3DBMapper();
    }

    @Override
    protected ConsoleDBMapper<ConsolidationDetailsResponse> getConsoleDBMapperTo() {
        return new ConsoleV2DBMapper();
    }

    @Override
    protected IDTOMigrationMapper<ConsolidationDetailsV3Response, ConsolidationDetailsResponse> getConsoleDTOMapper() {
        return new ConsolidationDTOV3ToV2Migration(new ConsolidationDTOV2ToV3Mapper());
    }

    @Override
    protected AbstractShipmentMigration<ShipmentDetailsV3Response, ShipmentDetailsResponse> getShipmentMapping() {
        return migrationV3ToV2;
    }
}
