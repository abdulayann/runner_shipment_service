package com.dpw.runner.shipment.services.migration.map.shipment.v2ToV3;

import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.AbstractShipmentMigration;
import com.dpw.runner.shipment.services.migration.map.shipment.ShipmentDBMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.v3ToV2.ShipmentV3DBMapper;
import org.springframework.stereotype.Service;

@Service
public class ShipmentMigrationV2ToV3 extends AbstractShipmentMigration<ShipmentDetailsResponse, ShipmentDetailsV3Response> {
    @Override
    protected ShipmentDBMapper<ShipmentDetailsResponse> getShipmentDBMapperFrom() {
        return new ShipmentV2DBMapper();
    }

    @Override
    protected ShipmentDBMapper<ShipmentDetailsV3Response> getShipmentDBMapperTo() {
        return new ShipmentV3DBMapper();
    }

    @Override
    protected IDTOMigrationMapper<ShipmentDetailsResponse, ShipmentDetailsV3Response> getShipmentDTOMapper() {
        return new ShipmentDTOV2ToV3Migration();
    }
}
