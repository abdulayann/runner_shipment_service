package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class PackSummaryRequest implements IRunnerRequest {
    private String totalPacks;
    private String totalPacksWeight;
    private String totalPacksVolume;
    private String packsVolumetricWeight;
    private String packsChargeableWeight;
}
