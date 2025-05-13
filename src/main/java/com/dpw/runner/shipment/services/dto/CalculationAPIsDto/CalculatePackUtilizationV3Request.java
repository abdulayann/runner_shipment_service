package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.AllocationsRequest;
import com.dpw.runner.shipment.services.dto.v3.request.PackingV3Request;
import com.dpw.runner.shipment.services.entity.Packing;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CalculatePackUtilizationV3Request implements IRunnerRequest {
    private List<PackingV3Request> packingList;

    // extra fields for inter branch pack updates
    private List<Long> shipmentIdList;
    private Long consolidationId;
    private List<Packing> shipmentPackingList;
    private AllocationsRequest allocationsRequest;
    private Boolean ignoreConsolidationPacks;

    private Boolean saveConsol;
    private Boolean isHub;
}
