package com.dpw.runner.shipment.services.syncing;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.syncing.Entity.AuditLogRequestV2;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class AuditLogsSyncRequest implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("ChangeLogs")
    private List<AuditLogRequestV2> ChangeLogs;
}
