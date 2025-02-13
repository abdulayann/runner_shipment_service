package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Data;

import java.util.List;

@Data
public class PreAlertModel implements IDocumentModel {
    public ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public ConsolidationModel consolidationDetails;
    public String noofpackages_word;
    public String userdisplayname;
    public ShipmentSettingsDetails shipmentSettingsDetails;
    private List<ShipmentContainers> shipmentContainers;
}
