package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class ChargeTypesResponse {
    @JsonProperty("Services")
    private String services; // TODO: SUBHAM which field to map to in Billing service?
}
