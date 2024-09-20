package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class CheckAllocatedDataChangeResponse implements IRunnerResponse {
    private Boolean volumeAllowed;
    private Boolean weightAllowed;
}
