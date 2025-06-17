package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class TruckDriverDetailsModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("TransporterType")
    private Ownership transporterType;
    @JsonProperty("ThirdPartyTransporter")
    private Parties thirdPartyTransporter;
    @JsonProperty("TransporterName")
    private String transporterName;
    @JsonProperty("DriverName")
    private String driverName;
    @JsonProperty("DriverMobileNumber")
    private String driverMobileNumber;
    @JsonProperty("TruckNumberPlate")
    private String truckNumberPlate;
    @JsonProperty("TrailerNumberPlate")
    private String trailerNumberPlate;
    @JsonProperty("TruckOrTrailerType")
    private String truckOrTrailerType;
    @JsonProperty("ContainerTypeCode")
    private String containerTypeCode;
    @JsonProperty("ContainerId")
    private Long containerId;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("SelfTransporterName")
    private String selfTransporterName;
    @JsonProperty("DriverID")
    private String driverId;
}
