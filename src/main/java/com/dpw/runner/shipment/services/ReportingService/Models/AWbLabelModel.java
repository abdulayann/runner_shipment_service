package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

import java.util.List;

@Data
public class AWbLabelModel implements IDocumentModel{
    public TenantModel tenant;
    public ShipmentModel shipment;
    private List<String> TenantAddress;
}
