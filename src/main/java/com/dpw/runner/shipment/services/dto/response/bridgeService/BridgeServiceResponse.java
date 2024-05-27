package com.dpw.runner.shipment.services.dto.response.bridgeService;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.Map;

@Data
public class BridgeServiceResponse implements IRunnerResponse {
    private String tenantCode;
    private String transactionId;
    private String requestCode;
    private Object payload;
    private Map<String, Object> extraResponseParams;
}
