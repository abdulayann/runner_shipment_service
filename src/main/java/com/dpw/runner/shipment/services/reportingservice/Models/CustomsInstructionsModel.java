package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

import java.util.List;

@Data
public class CustomsInstructionsModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    private List<ShipmentContainers> shipmentContainers;
}
