package com.dpw.runner.shipment.services.dto.response.bridgeService;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@SuppressWarnings("java:S1948")
public class BridgeServiceResponse implements IRunnerResponse {
    private String tenantCode;
    private String transactionId;
    private String requestCode;
    private Object payload;
    private Map<String, Object> extraResponseParams;
}
