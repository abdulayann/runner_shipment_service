package com.dpw.runner.shipment.services.migration.map.shipment.v2ToV3;

import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.migration.map.shipment.ShipmentDBMapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ShipmentV2DBMapper extends ShipmentDBMapper<ShipmentDetailsResponse> {
    @Override
    public ShipmentDetails mapToEntity(ShipmentDetailsResponse shipmentDetailsResponse) {
        ShipmentDetails detail = new ShipmentDetails();
        detail.setId(shipmentDetailsResponse.getId());
        log.info("Mapping Shipment V2 DTO to Shipment Entity  for Id {}", shipmentDetailsResponse.getId());
        return detail;
    }

    @Override
    public ShipmentDetailsResponse mapFromEntity(ShipmentDetails d) {
        ShipmentDetailsResponse detail = new ShipmentDetailsResponse();
        detail.setId(d.getId());
        log.info("Mapping   Shipment Entity to  Shipment V2 DTO for Id {}", d.getId());
        return detail;
    }
}
