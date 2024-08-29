package com.dpw.runner.shipment.services.dto.v1.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

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

    @JsonProperty("EntityID")
    private String entityId;

    @JsonProperty("EntityType")
    private String entityType;

    @JsonProperty("Remarks")
    private String tenantId;

    @JsonProperty("Remarks")
    private String taskJson;

    @JsonProperty("TaskType")
    private String taskType;

    @JsonProperty("RejectionRemarks")
    private String rejectionRemarks;

    @JsonProperty("UserId")
    private Integer userId;

    @JsonProperty("UserName")
    private String userName;

    @JsonProperty("SendMail")
    private Boolean sendEmail;

//    {
//        "EntityType": "[Shipments]",
//            "TaskType": "Payment Approval",
//            "TaskInfoJson": "{\"name\":\"John\",\"age\":30,\"city\":\"New York\"}",
//            "EntityId": "116111",
//            "TenantId": "23",
//            "UserId": "715",
//            "UserName": "JohnDoe",
//            "TaskStatus": "Approved",
//            "RoleId": null,
//            "SequenceNumber": null,
//            "QuoteRejectionRemarks": null,
//            "RejectionRemarks": null,
//            "Remarks": null,
//            "SendMail": false
//    }
}
