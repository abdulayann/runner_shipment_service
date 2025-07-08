package com.dpw.runner.shipment.services.migration.map.console.v2ToV3;

import com.dpw.runner.shipment.services.dto.response.ConsolidationDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ConsolidationDetailsV3Response;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import com.dpw.runner.shipment.services.migration.map.console.AbstractConsolidationMigration;
import com.dpw.runner.shipment.services.migration.map.console.ConsoleDBMapper;
import com.dpw.runner.shipment.services.migration.map.console.v3ToV2.ConsoleV3DBMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.AbstractShipmentMigration;
import com.dpw.runner.shipment.services.migration.map.shipment.v2ToV3.ShipmentMigrationV2ToV3;
import org.springframework.stereotype.Service;


@Service
public class ConsolidationV2ToV3Migration extends AbstractConsolidationMigration<ConsolidationDetailsResponse, ConsolidationDetailsV3Response, ShipmentDetailsResponse, ShipmentDetailsV3Response> {

    //@Autowired
    private ShipmentMigrationV2ToV3 migrationV2ToV3 = new ShipmentMigrationV2ToV3();

    @Override
    protected ConsoleDBMapper<ConsolidationDetailsResponse> getConsoleDBMapperFrom() {
        return new ConsoleV2DBMapper();
    }

    @Override
    protected ConsoleDBMapper<ConsolidationDetailsV3Response> getConsoleDBMapperTo() {
        return new ConsoleV3DBMapper();
    }

    @Override
    protected IDTOMigrationMapper<ConsolidationDetailsResponse, ConsolidationDetailsV3Response> getConsoleDTOMapper() {
        return new ConsolidationDTOV2ToV3Mapper();
    }

    @Override
    protected AbstractShipmentMigration<ShipmentDetailsResponse, ShipmentDetailsV3Response> getShipmentMapping() {
        return migrationV2ToV3;
    }
}

