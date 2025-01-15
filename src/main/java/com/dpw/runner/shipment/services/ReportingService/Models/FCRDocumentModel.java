package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

@Data
public class FCRDocumentModel implements IDocumentModel {
    private ShipmentModel shipmentModel;
}
