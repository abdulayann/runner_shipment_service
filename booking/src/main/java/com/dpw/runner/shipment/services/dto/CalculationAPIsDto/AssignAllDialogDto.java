package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

@Data
public class AssignAllDialogDto implements IRunnerResponse {
    private long numberOfShipments;
    private boolean showDialog;
}
