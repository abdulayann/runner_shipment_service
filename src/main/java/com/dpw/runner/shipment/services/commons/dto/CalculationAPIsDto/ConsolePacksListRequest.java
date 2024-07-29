package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class ConsolePacksListRequest implements IRunnerRequest {
    private Boolean isAssign;
    private Long consolidationId;
    private Long containerId;
}
