package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AchievedQuantities {
    @JsonProperty("ConsolidatiedVolume")
    public BigDecimal ConsolidatiedVolume;
    @JsonProperty("ConsolidatiedVolumeUnit")
    public String ConsolidatiedVolumeUnit;
    @JsonProperty("ConsolidatiedWeight")
    public BigDecimal ConsolidatiedWeight;
    @JsonProperty("ConsolidatiedWeightUnit")
    public String ConsolidatiedWeightUnit;
    @JsonProperty("ConsolidationChargeQuantityUnit")
    public String ConsolidationChargeQuantityUnit;
    @JsonProperty("ConsolidationChargeQuantity")
    public BigDecimal ConsolidationChargeQuantity;
    @JsonProperty("Guid")
    public UUID Guid;
    @JsonProperty("VolumeUtilization")
    public String VolumeUtilization;
    @JsonProperty("WeightUtilization")
    public String WeightUtilization;
    @JsonProperty("WeightVolumeUnit")
    public String WeightVolumeUnit;
    @JsonProperty("WeightVolume")
    public BigDecimal WeightVolume;
}
