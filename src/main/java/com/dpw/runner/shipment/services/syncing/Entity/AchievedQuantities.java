package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AchievedQuantities {
    private BigDecimal ConsolidatiedVolume;
    private String ConsolidatiedVolumeUnit;
    private BigDecimal ConsolidatiedWeight;
    private String ConsolidatiedWeightUnit;
    private String ConsolidationChargeQuantityUnit;
    private BigDecimal ConsolidationChargeQuantity;
    private UUID Guid;
    private Long Id;
    private String VolumeUtilization;
    private String WeightUtilization;
    private String WeightVolumeUnit;
    private BigDecimal WeightVolume;
}
