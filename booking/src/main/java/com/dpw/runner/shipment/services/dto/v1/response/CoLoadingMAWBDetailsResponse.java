package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CoLoadingMAWBDetailsResponse implements Serializable {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ParentTenantId")
    private Integer parentTenantId;
    @JsonProperty("ChildTenantId")
    private Integer childTenantId;
}