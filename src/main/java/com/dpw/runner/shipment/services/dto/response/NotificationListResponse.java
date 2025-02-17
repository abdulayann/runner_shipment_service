package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.NotificationRequestType;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
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
public class NotificationListResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private String entityType;
    private Long entityId;
    private Long requestedBranchId;
    private String requestedUser;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime requestedOn;
    private NotificationRequestType notificationRequestType;
    private String reason;
    private Long reassignedToBranchId;
    private Map<String, String> tenantMasterData;
    private Integer tenantId;
}
