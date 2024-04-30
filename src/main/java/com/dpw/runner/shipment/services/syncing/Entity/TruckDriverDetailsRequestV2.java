package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class TruckDriverDetailsRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("ShipmentGuid")
    private UUID ShipmentGuid;
    @JsonProperty("ConsolidationGuid")
    private UUID ConsolidationGuid;
    @JsonProperty("ContainerGuid")
    private UUID ContainerGuid;
    @JsonProperty("ThirdPartyTransporter")
    private PartyRequestV2 ThirdPartyTransporter;
    @JsonProperty("ContainerTypeCode")
    private String ContainerTypeCode;
    @JsonProperty("DriverMobileNumber")
    private String DriverMobileNumber;
    @JsonProperty("DriverName")
    private String DriverName;
    @JsonProperty("SelfTransporterName")
    private String SelfTransporterName;
    @JsonProperty("TrailerNumberPlate")
    private String TrailerNumberPlate;
    @JsonProperty("TransporterTypeString")
    private String TransporterTypeString;
    @JsonProperty("TruckNumberPlate")
    private String TruckNumberPlate;
    @JsonProperty("TruckOrTrailerType")
    private String TruckOrTrailerType;
    @JsonProperty("Remarks")
    private String Remarks;
    @JsonProperty("TruckStatus")
    private String TruckStatus;
}
