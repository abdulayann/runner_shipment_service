package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class V1TenantResponse {
    @JsonProperty("TenantName")
    private String TenantName;
    @JsonProperty("DisplayName")
    private String DisplayName;
    @JsonProperty("Code")
    private String Code;
    @JsonProperty("TenantId")
    private Long TenantId;
}
