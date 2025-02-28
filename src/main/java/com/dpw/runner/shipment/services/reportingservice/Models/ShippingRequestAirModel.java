package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

import java.util.List;

@Data
public class ShippingRequestAirModel implements IDocumentModel{
    public ShipmentModel shipment;
    private List<PackingModel> shipmentPacking;
}
