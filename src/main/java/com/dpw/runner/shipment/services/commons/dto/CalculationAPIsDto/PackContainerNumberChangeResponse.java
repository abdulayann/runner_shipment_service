package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.dto.response.ContainerResponse;
import lombok.Data;

@Data
public class PackContainerNumberChangeResponse implements IRunnerResponse {
    private ContainerResponse oldContainer;
    private ContainerResponse newContainer;
}
