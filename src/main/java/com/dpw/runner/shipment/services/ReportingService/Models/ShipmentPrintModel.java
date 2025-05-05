package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

@Data
public class ShipmentPrintModel implements IDocumentModel{

    public ShipmentModel shipmentDetails;
    public String noOfPackagesWord;
    public String userDisplayName;
    public TenantModel tenant;
    public Boolean isMultipleCopy;
    public Short originalCount;
    public Short copyCount;
    public String originalOrCopy;
    public String releaseType;
    public String openDateFormatted;
    public String vesselName;
    public Integer noOfContainers;
    public Integer totalRecords;
    public String secondPartOfDesc;

}
