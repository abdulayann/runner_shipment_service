package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class ReferenceNumbersRequestV2 implements IRunnerRequest {
    @JsonProperty("CountryOfIssue")
    public String CountryOfIssue;
    @JsonProperty("Type")
    public String Type;
    @JsonProperty("ReferenceNumber")
    public String ReferenceNumber;
    @JsonProperty("IsPortalEnable")
    public Boolean IsPortalEnable;
    @JsonProperty("Guid")
    private UUID Guid;
}
