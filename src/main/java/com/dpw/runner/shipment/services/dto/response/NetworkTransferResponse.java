package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferSource;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
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
    @SuppressWarnings("java:S1948")
    private Map<String, Object> entityPayload;
    private Map<String, String> masterData;
    private Map<String, String> tenantIdsData;
    private Integer tenantId;
    private LocalDateTime createdAt;
    private Boolean isInterBranchEntity;
    private Boolean isHidden;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime transferredDate;
    private NetworkTransferSource source;
}
