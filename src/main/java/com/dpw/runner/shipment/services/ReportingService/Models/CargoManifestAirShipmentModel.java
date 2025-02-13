package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.Awb;
import lombok.Data;

@Data
public class CargoManifestAirShipmentModel implements IDocumentModel {
    private ShipmentModel shipmentDetails;
    private TenantModel tenantModel;
    private Awb awb;
}
