package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;

public class MawbModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public UsersDto usersDto;
}
