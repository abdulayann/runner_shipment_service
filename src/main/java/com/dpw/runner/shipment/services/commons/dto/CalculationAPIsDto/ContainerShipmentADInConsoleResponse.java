package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.dto.response.ContainerResponse;
import lombok.Data;

@Data
public class ContainerShipmentADInConsoleResponse implements IRunnerResponse {
    private ContainerResponse container;
    private Boolean requireConfirmationFromUser;
}
