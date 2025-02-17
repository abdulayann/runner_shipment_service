package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.UUID;

import lombok.Data;

@Data
public class BillingResponse {

    @JsonProperty("Guid")
    @JsonAlias({"guId"})
    private UUID guid;

    @JsonProperty("billId")
    private String billId;

    @JsonProperty("Remarks")
    private String remarks;
}
