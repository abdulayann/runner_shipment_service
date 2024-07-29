package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class GenerateCustomHblResponse implements IRunnerResponse {
    private String hblNumber;
}
