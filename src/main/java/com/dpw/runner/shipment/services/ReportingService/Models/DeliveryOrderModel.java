package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.Hbl;

import java.util.List;

public class DeliveryOrderModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public List<ShipmentContainers> containers;
    public ConsolidationModel consolidationDetails;
    public UsersDto usersDto;
    public String paymentTerms;
    public String placeOfIssueName;
    public Hbl hbl;
}
