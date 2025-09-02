package com.dpw.runner.shipment.services.dto.response.mdm;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferShipmentDetails;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class MDMTaskRetrieveResponse implements IRunnerResponse {
    @JsonProperty("id")
    private Long id;

    @JsonProperty("uuid")
    private String guid;

    @JsonProperty("status")
    private String status;

    @JsonProperty("roleId")
    private String roleId;

    @JsonProperty("entityID")
    private String entityId;

    @JsonProperty("entityUuid")
    private String entityGuid;

    @JsonProperty("entityType")
    private String entityType;

    @JsonProperty("tenantId")
    private String tenantId;

    @JsonProperty("taskInfoJson")
    private String taskJson;

    @JsonProperty("consoleJson")
    private EntityTransferConsolidationDetails consoleJson;

    @JsonProperty("shipmentJson")
    private EntityTransferShipmentDetails shipmentJson;

    @JsonProperty("taskType")
    private String taskType;

    @JsonProperty("rejectionRemarks")
    private String rejectionRemarks;

    @JsonProperty("userId")
    private Long userId;

    @JsonProperty("requesterUserName")
    private String userName;

    @JsonProperty("userEmail")
    private String userEmail;

    @JsonProperty("sendMail")
    private Boolean sendEmail;

    @JsonProperty("isActive")
    private Boolean isActive;

    @JsonProperty("isCreatedFromV2")
    private Boolean isCreatedFromV2;
}
