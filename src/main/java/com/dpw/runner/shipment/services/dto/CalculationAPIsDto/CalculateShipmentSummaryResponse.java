package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CalculateShipmentSummaryResponse implements IRunnerResponse {
    private String totalPacksWeight;
    private String totalPacksVolume;
    private String packsChargeableWeight;
    private String totalPacksWithUnit;
}
