package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import lombok.Data;

import java.util.List;

@Data
public class CalculatePackSummaryRequest implements IRunnerRequest {
    private List<PackingRequest> packingList;
    private String transportMode;
    private String shipmentType;
    private String containerCategory;
}
