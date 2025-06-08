package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import java.util.List;
import lombok.Data;

@Data
public class CalculatePackSummaryRequest implements IRunnerRequest {
    private List<PackingRequest> packingList;
    private String transportMode;
    private String shipmentType;
    private String containerCategory;
    private Long shipmentEntityId;
    // extra fields for inter branch pack updates
    private ShipmentRequest shipmentRequest;
    private Long consolidationId;
}
