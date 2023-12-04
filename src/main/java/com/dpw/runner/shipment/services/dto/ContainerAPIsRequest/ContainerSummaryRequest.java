package com.dpw.runner.shipment.services.dto.ContainerAPIsRequest;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import lombok.Data;

import java.util.List;

@Data
public class ContainerSummaryRequest implements IRunnerRequest {
    private List<ContainerRequest> containerRequestList;
    private String transportMode;
    private String shipmentType;
    private String containerCategory;
}
