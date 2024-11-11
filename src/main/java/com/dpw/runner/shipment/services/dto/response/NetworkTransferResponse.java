package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Network Transfer Response")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class NetworkTransferResponse implements IRunnerResponse {
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
    private Map<String, String> masterData;
    private Map<String, String> tenantIdsData;
    private Integer tenantId;
    private LocalDateTime createdAt;
}
