package com.dpw.runner.shipment.services.migration.map.shipment.v2ToV3;

import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.migration.IDTOMigrationMapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ShipmentDTOV2ToV3Migration implements IDTOMigrationMapper<ShipmentDetailsResponse, ShipmentDetailsV3Response> {
    @Override
    public ShipmentDetailsV3Response forwardMap(ShipmentDetailsResponse shipmentDetailsResponse) {
        ShipmentDetailsV3Response v3Response = new ShipmentDetailsV3Response();
        v3Response.setId(shipmentDetailsResponse.getId());
        log.info(" Mapping Shipment V2 DTO to Shipment V3 DTO for Id {}", v3Response.getId());
        return v3Response;
    }

    @Override
    public ShipmentDetailsResponse reverseMap(ShipmentDetailsV3Response shipmentDetailsV3Response) {
        ShipmentDetailsResponse v3Response = new ShipmentDetailsResponse();
        v3Response.setId(shipmentDetailsV3Response.getId());
        log.info("Mapping Shipment V3 DTO to Shipment V2 DTO for Id {}", v3Response.getId());
        return v3Response;
    }
}
