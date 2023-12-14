package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;

import java.util.List;

public class PreAlertModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public ConsolidationModel consolidationDetails;
    public List<ShipmentContainers> shipmentContainers;
    public String noofpackages_word;
    public String userdisplayname;
}
