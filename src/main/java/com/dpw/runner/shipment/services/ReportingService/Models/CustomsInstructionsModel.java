package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

import java.util.List;

@Data
public class CustomsInstructionsModel implements IDocumentModel {
    public ShipmentModel shipmentDetails;
    private List<ShipmentContainers> shipmentContainers;
}
