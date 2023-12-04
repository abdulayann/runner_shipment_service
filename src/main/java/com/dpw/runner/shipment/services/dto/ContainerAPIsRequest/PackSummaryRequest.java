package com.dpw.runner.shipment.services.dto.ContainerAPIsRequest;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import lombok.Data;

import java.util.List;

@Data
public class PackSummaryRequest implements IRunnerRequest {
    private List<PackingRequest> packingRequestList;
    private String transportMode;
    private String shipmentType;
    private String containerCategory;
}
