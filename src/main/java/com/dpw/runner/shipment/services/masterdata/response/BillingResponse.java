package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class BillingResponse {
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("Remarks")
    private String remarks;
}
