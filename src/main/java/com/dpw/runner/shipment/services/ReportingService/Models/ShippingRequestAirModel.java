package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

import java.util.List;

@Data
public class ShippingRequestAirModel implements IDocumentModel{
    public ShipmentModel shipment;
    public List<PackingModel> shipmentPacking;
}
