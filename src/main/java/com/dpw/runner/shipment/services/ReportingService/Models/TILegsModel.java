package com.dpw.runner.shipment.services.ReportingService.Models;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
public class TILegsModel implements Serializable {
    private Long id;
    private UUID guid;
    private Long sequence;
    @JsonProperty("TI_Leg_Type")
    private String legType;
    @JsonProperty("TI_Origin")
    private Object origin;
    @JsonProperty("TI_Origin_Address")
    private String originAddress;
    @JsonProperty("TI_Destination")
    private Object destination;
    @JsonProperty("TI_Destination_Address")
    private String destinationAddress;
    @JsonProperty("TI_Estimated_Pickup")
    private LocalDateTime estimatedPickup;
    @JsonProperty("TI_Estimated_Delivery")
    private LocalDateTime estimatedDelivery;
    @JsonProperty("TI_Required_By")
    private LocalDateTime requiredBy;
    @JsonProperty("TI_Drop_Mode")
    private String dropMode;
    @JsonProperty("TI_Remarks")
    private String remarks;
}
