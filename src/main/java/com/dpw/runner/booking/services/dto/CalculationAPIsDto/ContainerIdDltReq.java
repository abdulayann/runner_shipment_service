package com.dpw.runner.booking.services.dto.CalculationAPIsDto;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class ContainerIdDltReq implements IRunnerRequest {
    private Long id;
}
