package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class Allocations implements IRunnerRequest {
    @JsonProperty("Chargable")
    private BigDecimal Chargable;
    @JsonProperty("ChargeableUnit")
    private String ChargeableUnit;
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("CutoffDate")
    private LocalDateTime CutoffDate;
    @JsonProperty("Hazardous")
    private Boolean Hazardous;
    @JsonProperty("IsTemparatureControlled")
    private Boolean IsTemparatureControlled;
    @JsonProperty("MaxTemp")
    private BigDecimal MaxTemp;
    @JsonProperty("MaxTempUnit")
    private String MaxTempUnit;
    @JsonProperty("MinTemp")
    private BigDecimal MinTemp;
    @JsonProperty("MinTempUnit")
    private String MinTempUnit;
    @JsonProperty("ShipmentsCount")
    private Long ShipmentsCount;
    @JsonProperty("Volume")
    private BigDecimal Volume;
    @JsonProperty("VolumeUnit")
    private String VolumeUnit;
    @JsonProperty("WeightUnit")
    private String WeightUnit;
    @JsonProperty("Weight")
    private BigDecimal Weight;
}
