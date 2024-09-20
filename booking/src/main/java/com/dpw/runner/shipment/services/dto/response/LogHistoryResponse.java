package com.dpw.runner.shipment.services.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class LogHistoryResponse {
    private Long id;
    private UUID guid;
    private long entityId;
    private String entityType;
    private UUID entityGuid;
    private String entityPayload;
}
