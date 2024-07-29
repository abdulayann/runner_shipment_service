package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.commons.dto.response.AllocationsResponse;
import lombok.Data;

@Data
public class ConsoleCalculationsResponse implements IRunnerResponse {
    private AchievedQuantitiesResponse achievedQuantities;
    private AllocationsResponse allocations;
}
