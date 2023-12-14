package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;

import java.util.List;

public class CustomsInstructionsModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public List<ShipmentContainers> shipmentContainers;
}
