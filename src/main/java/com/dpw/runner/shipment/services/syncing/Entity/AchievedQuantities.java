package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AchievedQuantities implements IRunnerRequest {
    @JsonProperty("ConsolidatiedVolume")
    private BigDecimal ConsolidatiedVolume;
    @JsonProperty("ConsolidatiedVolumeUnit")
    private String ConsolidatiedVolumeUnit;
    @JsonProperty("ConsolidatiedWeight")
    private BigDecimal ConsolidatiedWeight;
    @JsonProperty("ConsolidatiedWeightUnit")
    private String ConsolidatiedWeightUnit;
    @JsonProperty("ConsolidationChargeQuantityUnit")
    private String ConsolidationChargeQuantityUnit;
    @JsonProperty("ConsolidationChargeQuantity")
    private BigDecimal ConsolidationChargeQuantity;
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("VolumeUtilization")
    private String VolumeUtilization;
    @JsonProperty("WeightUtilization")
    private String WeightUtilization;
    @JsonProperty("WeightVolumeUnit")
    private String WeightVolumeUnit;
    @JsonProperty("WeightVolume")
    private BigDecimal WeightVolume;
}
