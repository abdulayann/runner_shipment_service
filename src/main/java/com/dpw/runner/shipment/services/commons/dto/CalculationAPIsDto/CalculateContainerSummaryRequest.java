package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.dto.request.ContainerRequest;
import lombok.Data;

import java.util.List;

@Data
public class CalculateContainerSummaryRequest implements IRunnerRequest {
    private List<ContainerRequest> containersList;
    private String transportMode;
    private String shipmentType;
    private String containerCategory;
}
