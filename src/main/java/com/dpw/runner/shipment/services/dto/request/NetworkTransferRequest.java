package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferSource;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;

@Getter
@Setter
@ApiModel(value = "Network request model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class NetworkTransferRequest implements IRunnerRequest {
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
    private Integer tenantId;
    private LocalDateTime createdAt;
    private Boolean isInterBranchEntity;
    private Boolean isHidden;
    private LocalDateTime transferredDate;
    private NetworkTransferSource source;
}
