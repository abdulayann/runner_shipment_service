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
@SuppressWarnings("java:S1948")
public class TaskUpdateRequest implements IRunnerRequest {

    @JsonProperty("Id")
    private String id;

    @JsonProperty("Status")
    private String status;

    @JsonProperty("RejectionRemarks")
    private String rejectionRemarks;

}