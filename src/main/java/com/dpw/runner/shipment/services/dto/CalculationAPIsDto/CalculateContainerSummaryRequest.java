package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import lombok.Data;

import java.util.List;

@Data
public class CalculateContainerSummaryRequest implements IRunnerRequest {
    private List<ContainerRequest> containersList;
    private String transportMode;
    private String shipmentType;
    private String containerCategory;
}
