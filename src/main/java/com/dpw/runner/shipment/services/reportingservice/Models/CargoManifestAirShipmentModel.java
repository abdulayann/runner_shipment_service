package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.Awb;
import lombok.Data;

@Data
public class CargoManifestAirShipmentModel implements IDocumentModel{
    private ShipmentModel shipmentDetails;
    private TenantModel tenantModel;
    private Awb awb;
}
