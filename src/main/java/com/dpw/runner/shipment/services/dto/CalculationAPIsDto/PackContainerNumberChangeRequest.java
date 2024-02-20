package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import lombok.Data;

@Data
public class PackContainerNumberChangeRequest implements IRunnerRequest {
    private ContainerRequest oldContainer;
    private ContainerRequest newContainer;
    private PackingRequest newPack;
    private PackingRequest oldPack;
}
