package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class CheckAllocatedDataChangesRequest implements IRunnerRequest {
    private BigDecimal allocatedWeight;
    private String allocatedWeightUnit;
    private BigDecimal allocatedVolume;
    private String allocatedVolumeUnit;
    private String containerCode;
}
