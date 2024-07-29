package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.commons.dto.request.PackingRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PackContainerNumberChangeRequest implements IRunnerRequest {
    private ContainerRequest oldContainer;
    private ContainerRequest newContainer;
    private PackingRequest newPack;
    private PackingRequest oldPack;
}
