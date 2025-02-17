package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import lombok.Data;

@Data
public class ShipmentPrintModel implements IDocumentModel {

    private final String basePath = "Upload/";

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
    //    public BillRow firstBill;
    public Integer totalRecords;
    public String secondPartOfDesc;

//    private String getLogoPath(String type)
//    {
//        UsersDto user = UserContext.getUser();
//
//        var path = switch (type) {
//            case "Header" -> basePath + user.TenantPrintLogo;
//            case "HBL" -> basePath + user.HouseBillLogo;
//            default -> basePath + user.TenantId + "/Assets/";
//        };
//
//        return path;
//    }
//
//    public String Logo()
//    {
//        return getLogoPath("Header");
//    }
//
//    public String HouseBillLogo() {
//         return getLogoPath("HBL");
//    }

}
