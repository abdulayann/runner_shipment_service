package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

@Data
public class FCRDocumentModel implements IDocumentModel {
    private ShipmentModel shipmentModel;
}
