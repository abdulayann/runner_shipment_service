package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.Awb;
import lombok.Data;

import java.util.List;

@Data
public class AWbLabelModel implements IDocumentModel{
    public TenantModel tenant;
    public ShipmentModel shipment;
    private ConsolidationModel consolidation;
    private List<String> TenantAddress;
    private Awb awb;
    private String remarks;
//    public add awb
}
