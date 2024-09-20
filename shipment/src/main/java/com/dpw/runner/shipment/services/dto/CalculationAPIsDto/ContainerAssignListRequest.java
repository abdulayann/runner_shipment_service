package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ContainerAssignListRequest implements IRunnerRequest {
    Long shipmentId;
    Long consolidationId;
    Integer take;
    String transportMode;
}
