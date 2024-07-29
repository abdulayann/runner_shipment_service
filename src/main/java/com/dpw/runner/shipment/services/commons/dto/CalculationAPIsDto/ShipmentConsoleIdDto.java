package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class ShipmentConsoleIdDto implements IRunnerRequest {
    private Long shipmentId;
    private Long consolidationId;
}
