package com.dpw.runner.shipment.services.ReportingService.Models;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.TenantProducts;
import lombok.Data;

@Data
public class ShipmentPrintModel {

    private String GetLogoPath(String type)
    {
        UsersDto user = UserContext.getUser();

        var pathBase = "Upload/";
        var path = switch (type) {
            case "Header" -> pathBase + user.TenantPrintLogo;
            case "HBL" -> pathBase + user.HouseBillLogo;
            default -> pathBase + user.TenantId + "/Assets/";
        };

        return path;
    }

    public String Logo()
    {
        return GetLogoPath("Header");
    }

    public String HouseBillLogo() {
         return GetLogoPath("HBL");
    }
    public ShipmentDetails shipmentDetails;
    public String noOfPackagesWord;
    public String userDisplayName;
    public Integer tenant;
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
}
