package com.dpw.runner.shipment.services.dto.v1.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class TaskCreateRequest implements IRunnerRequest {

    @JsonProperty("Id")
    private String id;

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

    @JsonProperty("TaskInfoJson")
    private String taskJson;

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
}
