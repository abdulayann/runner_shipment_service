package com.dpw.runner.shipment.services.dto.ContainerAPIsRequest;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class ContainerSummary implements IRunnerResponse {
    private String totalPackages;
    private String totalContainers;
    private String totalWeight;
    private String totalTareWeight;
    private String chargeableWeight;
    private String totalContainerVolume;
}
