package com.dpw.runner.booking.services.dto.CalculationAPIsDto;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class ContainerSummaryRequest implements IRunnerRequest {
    private String totalPackages;
    private String totalContainers;
    private String totalWeight;
    private String totalTareWeight;
    private String chargeableWeight;
    private String totalContainerVolume;
    private String summary;
}
