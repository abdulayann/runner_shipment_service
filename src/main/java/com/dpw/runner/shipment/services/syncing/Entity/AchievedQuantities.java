package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AchievedQuantities {
    public BigDecimal ConsolidatiedVolume;
    public String ConsolidatiedVolumeUnit;
    public BigDecimal ConsolidatiedWeight;
    public String ConsolidatiedWeightUnit;
    public String ConsolidationChargeQuantityUnit;
    public BigDecimal ConsolidationChargeQuantity;
    public UUID Guid;
    public Long Id;
    public String VolumeUtilization;
    public String WeightUtilization;
    public String WeightVolumeUnit;
    public BigDecimal WeightVolume;
}
