package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class ReferenceNumbersRequestV2 {
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
//    public String EntityType;
//    public Long EntityId;
}
