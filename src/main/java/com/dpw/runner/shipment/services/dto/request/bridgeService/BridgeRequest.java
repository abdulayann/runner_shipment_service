package com.dpw.runner.shipment.services.dto.request.bridgeService;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class BridgeRequest implements IRunnerRequest {
    private String requestCode;
    private String transactionId;
    private String referenceId;
    private Object payload;
}
