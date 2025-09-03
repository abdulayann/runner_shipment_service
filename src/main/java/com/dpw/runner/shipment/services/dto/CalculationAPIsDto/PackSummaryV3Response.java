package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class PackSummaryV3Response extends PackSummaryResponse {
    private Long assignedPackageCount = 0L;
    private Long unassignedPackageCount = 0L;
}
