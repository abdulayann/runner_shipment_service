package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Network Transfer Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class NetworkTransferRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private String entityType;
    private String entityNumber;
    private Long entityId;
    private Long createdEntityId;
    private String transportMode;
    private Long branchId;
    private NetworkTransferStatus status;
    private String jobType;
    private String branchName;
    private Long roleId;
    private Map<String, Object> entityPayload;
    private Integer tenantId;
    private LocalDateTime createdAt;
}
