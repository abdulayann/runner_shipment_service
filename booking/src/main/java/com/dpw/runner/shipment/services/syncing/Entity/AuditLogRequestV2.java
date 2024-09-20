package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class AuditLogRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("Action")
    private String Action;
    @JsonProperty("Module")
    private String Module;
    @JsonProperty("ParentType")
    private String ParentType;
    @JsonProperty("Changes")
    private String Changes;
}
