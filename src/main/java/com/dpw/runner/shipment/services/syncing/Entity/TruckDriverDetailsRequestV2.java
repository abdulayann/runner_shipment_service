package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

@Data
public class TruckDriverDetailsRequestV2 {
    private String ContainerTypeCode;
    private String DriverMobileNumber;
    private String DriverName;
    private String SelfTransporterName;
    private String TrailerNumberPlate;
    private String TransporterNameOrg;
    private String TransporterTypeString;
    private String TruckNumberPlate;
    private String TruckOrTrailerType;
}
