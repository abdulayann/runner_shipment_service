package com.dpw.runner.shipment.services.dto.v1.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TenantDetailsByListRequest implements IRunnerRequest {
    @JsonProperty("TenantIds")
    private List<Integer> tenantIds;
    @JsonProperty("Take")
    private Integer take;
}