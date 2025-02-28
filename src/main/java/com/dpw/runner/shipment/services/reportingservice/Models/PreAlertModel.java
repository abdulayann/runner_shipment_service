package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Data;

import java.util.List;

@Data
public class PreAlertModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public ConsolidationModel consolidationDetails;
    private List<ShipmentContainers> shipmentContainers;
    public String noofpackages_word;
    public String userdisplayname;
    public ShipmentSettingsDetails shipmentSettingsDetails;
}
