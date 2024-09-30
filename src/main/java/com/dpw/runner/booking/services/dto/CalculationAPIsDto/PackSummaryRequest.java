package com.dpw.runner.booking.services.dto.CalculationAPIsDto;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class PackSummaryRequest implements IRunnerRequest {
    private String totalPacks;
    private String totalPacksWeight;
    private String totalPacksVolume;
    private String packsVolumetricWeight;
    private String packsChargeableWeight;
}
