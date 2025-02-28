package com.dpw.runner.shipment.services.reportingservice.Models;

import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import lombok.Data;

@Data
public class DocPages {
    private String firstPageId;
    private String mainPageId;
    private String backPrintId;
    private boolean isLogoFixed;
    private ShipmentSettingsDetails shipmentSettingsDetails;
}
