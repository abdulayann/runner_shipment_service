package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LogHistoryRequest {
    private long entityId;
    private String entityType;
    private UUID entityGuid;
    private String entityPayload;
    private Integer tenantId;
}
