package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;

import java.util.List;

public class HawbModel implements IDocumentModel{
    public ShipmentDetails shipmentDetails;
    public UsersDto usersDto;
    public List<Containers> containers;
}
