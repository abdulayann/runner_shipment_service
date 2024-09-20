package com.dpw.runner.shipment.services.dto.v1.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferShipmentDetails;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TaskResponse implements IRunnerResponse {
    @JsonProperty("Id")
    private Long id;

    @JsonProperty("Guid")
    private String guid;

    @JsonProperty("Status")
    private String status;

    @JsonProperty("RoleId")
    private String roleId;

    @JsonProperty("EntityID")
    private String entityId;

    @JsonProperty("EntityType")
    private String entityType;

    @JsonProperty("TenantId")
    private String tenantId;

    @JsonProperty("TaskInfoJson")
    private String taskJson;

    @JsonProperty("ConsoleJson")
    private EntityTransferConsolidationDetails consoleJson;

    @JsonProperty("ShipmentJson")
    private EntityTransferShipmentDetails shipmentJson;

    @JsonProperty("TaskType")
    private String taskType;

    @JsonProperty("RejectionRemarks")
    private String rejectionRemarks;

    @JsonProperty("UserId")
    private Long userId;

    @JsonProperty("UserName")
    private String userName;

    @JsonProperty("UserEmail")
    private String userEmail;

    @JsonProperty("SendMail")
    private Boolean sendEmail;

    @JsonProperty("IsActive")
    private Boolean isActive;

    @JsonProperty("IsCreatedFromV2")
    private Boolean isCreatedFromV2;
}
