package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Builder;
import lombok.Data;

@Data
@Builder

public class SeawayBillModel implements IDocumentModel {

    public ShipmentModel shipment;
    public ConsolidationModel consolidation;
    public TenantModel tenant;
    public Hbl blObject;
    public ShipmentSettingsDetails shipmentSettingsDetails;
    private Long id;

}
