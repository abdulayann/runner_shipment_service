package com.dpw.runner.shipment.services.migration.map.shipment.v3ToV2;

import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import com.dpw.runner.shipment.services.migration.map.shipment.v2ToV3.ShipmentDTOV2ToV3Migration;


public class ShipmentDTOV3ToV2Migration implements IDTOMigrationMapper<ShipmentDetailsV3Response, ShipmentDetailsResponse> {

    private ShipmentDTOV2ToV3Migration dtov2ToV3Migration;

    public ShipmentDTOV3ToV2Migration(ShipmentDTOV2ToV3Migration dtov2ToV3Migration) {
        this.dtov2ToV3Migration = dtov2ToV3Migration;
    }

    @Override
    public ShipmentDetailsResponse forwardMap(ShipmentDetailsV3Response shipmentDetailsV3Response) {
        return dtov2ToV3Migration.reverseMap(shipmentDetailsV3Response);
    }

    @Override
    public ShipmentDetailsV3Response reverseMap(ShipmentDetailsResponse shipmentDetailsResponse) {
        return dtov2ToV3Migration.forwardMap(shipmentDetailsResponse);
    }
}
