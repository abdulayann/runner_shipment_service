package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class NetworkTransferListResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String entityType;
    private String entityNumber;
    private Long entityId;
    private Long createdEntityId;
    private String transportMode;
    private Long sourceBranchId;
    private NetworkTransferStatus status;
    private String jobType;
    private Map<String, Object> entityPayload;
    private Integer tenantId;
    private LocalDateTime createdAt;
    private Boolean isInterBranchEntity;
    private Map<String, String> tenantMasterData;
    private Map<String, String> masterData;
    private Boolean isHidden;
}
