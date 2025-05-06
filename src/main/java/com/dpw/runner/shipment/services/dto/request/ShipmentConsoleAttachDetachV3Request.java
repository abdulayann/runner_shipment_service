package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import java.util.Set;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentConsoleAttachDetachV3Request implements IRunnerRequest {

    private ShipmentRequestedType shipmentRequestedType;
    private Long consolidationId;
    private Set<Long> shipmentIds;
    private String remarks;
    private boolean isFromConsolidation;
}
