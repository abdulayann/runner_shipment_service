package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Containers;

import java.util.List;

public class MawbModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public UsersDto usersDto;
    public List<Containers> containers;
}
