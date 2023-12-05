package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;

import java.util.List;

public class FreightCertificationModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public String noofpackages_word;
    public List<ShipmentContainers> allContainersList;
    public String userdisplayname;
}
