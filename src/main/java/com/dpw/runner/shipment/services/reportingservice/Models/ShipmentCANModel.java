package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;

public class ShipmentCANModel implements IDocumentModel{

    public ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public ConsolidationModel consolidationModel;
    public ShipmentSettingsDetails shipmentSettingsDetails;
    public V1TenantSettingsResponse tenantSettingsResponse;
    public boolean isHBL = true;
}
