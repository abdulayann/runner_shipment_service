package com.dpw.runner.shipment.services.service_bus.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;

@Data
public class ContainerUpdateRequest {
    @JsonProperty("TenantCode")
    private String tenantCode;
    private List<ContainerPayloadDetails> containers;
}
