package com.dpw.runner.shipment.services.dto.response.bridgeService;

import lombok.Data;

import java.util.Map;

@Data
public class BridgeServiceResponse {
    private String tenantCode;
    private String transactionId;
    private String requestCode;
    private Object payload;
}
