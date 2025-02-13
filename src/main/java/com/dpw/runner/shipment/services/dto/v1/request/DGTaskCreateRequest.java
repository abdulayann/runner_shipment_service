package com.dpw.runner.shipment.services.dto.v1.request;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class DGTaskCreateRequest {

    @JsonProperty("TaskStatus")
    private String taskStatus;

    @JsonProperty("RoleId")
    private String roleId;

    @JsonProperty("EntityId")
    private String entityId;

    @JsonProperty("EntityType")
    private String entityType;

    @JsonProperty("TenantId")
    private String tenantId;

    @JsonProperty("TaskType")
    private String taskType;

    @JsonProperty("UserId")
    private Long userId;

}
