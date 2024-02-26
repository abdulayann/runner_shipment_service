package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;

public class CargoManifestModel implements IDocumentModel{
    public transient ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public UsersDto usersDto;
}
