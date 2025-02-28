package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.reportingservice.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
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
