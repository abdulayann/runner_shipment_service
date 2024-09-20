package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
