package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class TenantProductsDto implements IRunnerRequest {
    @JsonProperty("ProductType")
    private String ProductType;
    @JsonProperty("Alias")
    private String Alias;
    @JsonProperty("Priority")
    private Integer Priority;
    @JsonProperty("Enabled")
    private Boolean Enabled;
    @JsonProperty("EnableGrouping")
    private Boolean EnableGrouping;
}

