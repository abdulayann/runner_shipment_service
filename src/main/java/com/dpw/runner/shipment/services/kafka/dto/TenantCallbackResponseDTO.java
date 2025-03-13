package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TenantCallbackResponseDTO {
    private String transactionId;
    private String requestCode;
    private Object payload;
    private String tenantCode;
    private BaseError error;
    private Map<String, Object> extraResponseParams;
    private String message;
}
