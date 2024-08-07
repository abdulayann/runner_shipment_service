package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.io.Serializable;

@Data
public class CoLoadingMAWBDetailsResponse implements Serializable {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ParentTenantId")
    private Integer parentTenantId;
    @JsonProperty("ChildTenantId")
    private Integer childTenantId;
}