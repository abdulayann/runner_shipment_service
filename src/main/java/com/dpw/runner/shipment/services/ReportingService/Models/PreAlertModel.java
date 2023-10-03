package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;

import java.util.List;

public class PreAlertModel implements IDocumentModel{
    public ShipmentDetails shipment;
    public ConsolidationDetails consolidation;
    public Hbl blObject;
    public List<ShipmentContainers> commonContainers;
}
