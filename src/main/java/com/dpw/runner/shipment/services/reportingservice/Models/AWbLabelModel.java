package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
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
    private List<ShipmentModel> shipmentModels; // used only for combi label
//    public add awb
}
