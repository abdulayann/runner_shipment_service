package com.dpw.runner.shipment.services.dto.request.bridgeService;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TactBridgePayload implements IRunnerRequest {
    private String origin;
}
