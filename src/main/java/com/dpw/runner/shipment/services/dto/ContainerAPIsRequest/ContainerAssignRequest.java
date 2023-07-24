package com.dpw.runner.shipment.services.dto.ContainerAPIsRequest;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ContainerAssignRequest implements IRunnerRequest {
    Long shipmentId;
    Long consolidationId;
}
