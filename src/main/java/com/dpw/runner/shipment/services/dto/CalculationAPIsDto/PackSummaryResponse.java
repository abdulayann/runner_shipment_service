package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class PackSummaryResponse implements IRunnerResponse {
    private String totalPacks;
    private String totalPacksWeight;
    private String totalPacksVolume;
    private String packsVolumetricWeight;
    private String packsChargeableWeight;
    private String totalPacksWithUnit;
    private BigDecimal chargeableWeight;
    private String packsChargeableWeightUnit;
    private BigDecimal packsVolume;
    private String packsVolumeUnit;

    // Consolidation achieved summary
    private BigDecimal achievedWeight;
    private BigDecimal achievedVolume;
    private String weightUnit;
    private String volumeUnit;
    private BigDecimal allocatedWeight;
    private BigDecimal allocatedVolume;
    private AllocationsResponse allocationsResponse;
    private AchievedQuantitiesResponse consolidationAchievedQuantities;
    private Integer dgPacks;
}
