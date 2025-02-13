package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

@Data
@Builder
@Getter
@Setter
public class V1DataSyncRequest {
    @JsonProperty("Module")
    private String module;
    @JsonProperty("Entity")
    private Object entity;

}
