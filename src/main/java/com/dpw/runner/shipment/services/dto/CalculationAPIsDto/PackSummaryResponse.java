package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class PackSummaryResponse implements IRunnerResponse {
    private String totalPacks;
    private String totalPacksWeight;
    private String totalPacksVolume;
    private String packsVolumetricWeight;
    private String packsChargeableWeight;

    // Consolidation achieved summary
    private BigDecimal achievedWeight;
    private BigDecimal achievedVolume;
    private AchievedQuantities consolidationAchievedQuantities;
}
