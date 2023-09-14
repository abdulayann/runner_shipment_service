package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;

public class CargoManifestModel implements IDocumentModel{
    public ShipmentDetails shipmentDetails;
    public TenantModel tenantDetails;
    public UsersDto usersDto;
}
