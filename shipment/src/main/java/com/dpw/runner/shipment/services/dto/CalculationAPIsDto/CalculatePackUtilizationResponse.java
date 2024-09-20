package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import lombok.Data;


@Data
public class CalculatePackUtilizationResponse implements IRunnerResponse {
    private String totalPacks;
    private String totalPacksWeight;
    private String totalPacksVolume;
    private String packsVolumetricWeight;
    private String packsChargeableWeight;
    private String totalPacksWithUnit;

    // Consolidation achieved summary
    private AllocationsResponse allocationsResponse;
    private AchievedQuantitiesResponse consolidationAchievedQuantities;
}
