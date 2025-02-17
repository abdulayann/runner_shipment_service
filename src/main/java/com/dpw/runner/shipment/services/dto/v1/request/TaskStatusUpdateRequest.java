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
public class TaskStatusUpdateRequest implements IRunnerRequest {

    @JsonProperty("EntityId")
    public String entityId;

    @JsonProperty("Entity")
    public EntityDetails entity;

    @Builder
    public static class EntityDetails {

        @JsonProperty("Status")
        public int status;

        @JsonProperty("RejectionRemarks")
        public String rejectionRemarks;
    }
}
