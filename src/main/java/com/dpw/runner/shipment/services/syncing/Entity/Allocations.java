package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

public class Allocations {
    @JsonProperty("Chargable")
    public BigDecimal Chargable;
    @JsonProperty("ChargeableUnit")
    public String ChargeableUnit;
    @JsonProperty("Guid")
    public UUID Guid;
    @JsonProperty("CutoffDate")
    public LocalDateTime CutoffDate;
    @JsonProperty("Hazardous")
    public Boolean Hazardous;
    @JsonProperty("IsTemparatureControlled")
    public Boolean IsTemparatureControlled;
    @JsonProperty("MaxTemp")
    public BigDecimal MaxTemp;
    @JsonProperty("MaxTempUnit")
    public String MaxTempUnit;
    @JsonProperty("MinTemp")
    public BigDecimal MinTemp;
    @JsonProperty("MinTempUnit")
    public String MinTempUnit;
    @JsonProperty("ShipmentsCount")
    public Long ShipmentsCount;
    @JsonProperty("Volume")
    public BigDecimal Volume;
    @JsonProperty("VolumeUnit")
    public String VolumeUnit;
    @JsonProperty("WeightUnit")
    public String WeightUnit;
    @JsonProperty("Weight")
    public BigDecimal Weight;
}
