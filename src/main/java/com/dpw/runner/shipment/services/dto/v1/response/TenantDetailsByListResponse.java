package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;

@Data
@Builder
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class TenantDetailsByListResponse {
    @JsonProperty("Entities")
    private List<TenantDetails> entities;

    @Builder
    @Data @AllArgsConstructor
    @NoArgsConstructor
    public static class TenantDetails {
        @JsonProperty("TenantId")
        private Integer tenantId;
        @JsonProperty("Tenant")
        private Object tenant;
        @JsonProperty("TenantSettings")
        private Object tenantSettings;
    }
}