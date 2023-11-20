package com.dpw.runner.shipment.services.dto.ContainerAPIsRequest;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class ContainerNumberCheckResponse implements IRunnerResponse {
    private boolean success;
    private int lastDigit;
}
