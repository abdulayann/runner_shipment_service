package com.dpw.runner.shipment.services.migration.map.shipment.v3ToV2;

import com.dpw.runner.shipment.services.dto.v3.response.ShipmentDetailsV3Response;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.migration.map.shipment.ShipmentDBMapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ShipmentV3DBMapper extends ShipmentDBMapper<ShipmentDetailsV3Response> {
    @Override
    public ShipmentDetails mapToEntity(ShipmentDetailsV3Response shipmentDetailsResponse) {
        ShipmentDetails detail = new ShipmentDetails();
        detail.setId(shipmentDetailsResponse.getId());
        log.info("Mapping Shipment V3 DTO to Shipment Entity  for Id {}", shipmentDetailsResponse.getId());
        return detail;
    }

    @Override
    public ShipmentDetailsV3Response mapFromEntity(ShipmentDetails d) {
        ShipmentDetailsV3Response detail = new ShipmentDetailsV3Response();
        detail.setId(d.getId());
        log.info("Mapping   Shipment Entity to  Shipment V3 DTO for Id {}", d.getId());
        return detail;
    }
}
