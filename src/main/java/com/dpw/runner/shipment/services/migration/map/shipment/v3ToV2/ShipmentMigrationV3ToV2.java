package com.dpw.runner.shipment.services.migration.map.shipment.v3ToV2;

import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.AbstractShipmentMigration;
import com.dpw.runner.shipment.services.migration.map.shipment.ShipmentDBMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.v2ToV3.ShipmentV2DBMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.v2ToV3.ShipmentDTOV2ToV3Migration;
import org.springframework.stereotype.Service;

@Service
public class ShipmentMigrationV3ToV2 extends AbstractShipmentMigration<ShipmentDetailsV3Response, ShipmentDetailsResponse> {
    @Override
    protected ShipmentDBMapper<ShipmentDetailsV3Response> getShipmentDBMapperFrom() {
        return new ShipmentV3DBMapper();
    }

    @Override
    protected ShipmentDBMapper<ShipmentDetailsResponse> getShipmentDBMapperTo() {
        return new ShipmentV2DBMapper();
    }

    @Override
    protected IDTOMigrationMapper<ShipmentDetailsV3Response, ShipmentDetailsResponse> getShipmentDTOMapper() {
        return new ShipmentDTOV3ToV2Migration(new ShipmentDTOV2ToV3Migration());
    }
}
