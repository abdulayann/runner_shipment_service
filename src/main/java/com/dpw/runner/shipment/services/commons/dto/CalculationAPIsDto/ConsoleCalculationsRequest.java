package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.dto.request.AchievedQuantitiesRequest;
import com.dpw.runner.shipment.services.commons.dto.request.AllocationsRequest;
import lombok.Data;

@Data
public class ConsoleCalculationsRequest implements IRunnerRequest {
    private Long id;
    private String containerCategory;
    private String transportMode;
    private AchievedQuantitiesRequest achievedQuantities;
    private AllocationsRequest allocations;
}
