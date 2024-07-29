package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class PackSummaryResponse implements IRunnerResponse {
    private String totalPacks;
    private String totalPacksWeight;
    private String totalPacksVolume;
    private String packsVolumetricWeight;
    private String packsChargeableWeight;
}
