package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;

public class ShipmentCANModel implements IDocumentModel{

    public ShipmentModel shipmentDetails;
    public TenantModel tenantModel;
    public ConsolidationModel consolidationModel;
    public ShipmentSettingsDetails shipmentSettingsDetails;
}
