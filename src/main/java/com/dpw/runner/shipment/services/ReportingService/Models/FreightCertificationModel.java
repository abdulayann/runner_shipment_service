package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.commons.entity.ShipmentSettingsDetails;
import lombok.Data;

import java.util.List;

@Data
public class FreightCertificationModel implements IDocumentModel{
    public ShipmentModel shipmentDetails;
    public TenantModel tenantDetails;
    public String noofpackages_word;
    private List<ShipmentContainers> allContainersList;
    public String userdisplayname;
    public ShipmentSettingsDetails shipmentSettingsDetails;
}
