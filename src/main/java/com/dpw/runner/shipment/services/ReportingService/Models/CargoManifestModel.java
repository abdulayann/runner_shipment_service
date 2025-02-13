package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;

public class CargoManifestModel implements IDocumentModel {
    public ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public UsersDto usersDto;
    public Awb awb;
    public ShipmentSettingsDetails shipmentSettingsDetails;
}
