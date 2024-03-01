package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class ReferenceNumbersRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("ConsolidationId")
    public Long ConsolidationId;
    @JsonProperty("CountryOfIssue")
    public String CountryOfIssue;
    @JsonProperty("Type")
    public String Type;
    @JsonProperty("ShipmentId")
    public Long ShipmentId;
    @JsonProperty("ReferenceNumber")
    public String ReferenceNumber;
    @JsonProperty("IsPortalEnable")
    public Boolean IsPortalEnable;
//    public String EntityType;
//    public Long EntityId;
}
