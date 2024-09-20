package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.AllocationsRequest;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.ShipmentRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CalculatePackUtilizationRequest implements IRunnerRequest {
    private List<PackingRequest> packingList;
    private String transportMode;
    private String shipmentType;
    private String containerCategory;

    // extra fields for inter branch pack updates
    private List<Long> shipmentIdList;
    private ShipmentRequest shipmentRequest;
    private Long consolidationId;
    private AllocationsRequest allocationsRequest;
    private Boolean ignoreConsolidationPacks;

    private Boolean saveConsol;
    private Boolean isHub;
}
