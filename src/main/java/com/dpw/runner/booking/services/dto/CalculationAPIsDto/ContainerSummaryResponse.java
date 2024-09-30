package com.dpw.runner.booking.services.dto.CalculationAPIsDto;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class ContainerSummaryResponse implements IRunnerResponse {
    private String totalPackages;
    private String totalContainers;
    private String totalWeight;
    private String totalTareWeight;
    private String chargeableWeight;
    private String totalContainerVolume;
    private String summary;
    private Integer dgContainers;
}
