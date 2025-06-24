package com.dpw.runner.shipment.services.ReportingService.Models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.UUID;

@Getter
@Setter
public class TILegsTruckDriverModel implements Serializable {
    @JsonProperty("TI_DriverID")
    private Long id;
    private UUID guid;
    private Long tiLegId;
    @JsonProperty("TI_DriverName")
    private String driverName;
    @JsonProperty("TI_MobileNumber")
    private String driverMobileNumber;
    @JsonProperty("TI_TruckNumberPlate")
    private String truckNumberPlate;
    @JsonProperty("TI_TrailerNumberPlate")
    private String trailerNumberPlate;
    @JsonProperty("TI_Truck_TrailerType")
    private String truckOrTrailerType;
}
