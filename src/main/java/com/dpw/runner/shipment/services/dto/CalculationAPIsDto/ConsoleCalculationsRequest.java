package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.AchievedQuantitiesRequest;
import com.dpw.runner.shipment.services.dto.request.AllocationsRequest;
import lombok.Data;

@Data
public class ConsoleCalculationsRequest implements IRunnerRequest {
    private Long id;
    private String containerCategory;
    private String transportMode;
    private AchievedQuantitiesRequest achievedQuantities;
    private AllocationsRequest allocations;
}
