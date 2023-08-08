package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;

import java.util.List;

public class DeliveryOrderModel implements IDocumentModel{
    public ShipmentDetails shipmentDetails;
    public List<ShipmentContainers> containers;
    public ConsolidationDetails consolidationDetails;
    public UsersDto usersDto;
    public String paymentTerms;
    public String placeOfIssueName;
    public Hbl hbl;
}
