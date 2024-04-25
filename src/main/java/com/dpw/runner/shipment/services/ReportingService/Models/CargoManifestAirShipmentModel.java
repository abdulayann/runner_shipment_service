package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.Awb;

public class CargoManifestAirShipmentModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public TenantModel tenantModel;
    public Awb awb;
}
