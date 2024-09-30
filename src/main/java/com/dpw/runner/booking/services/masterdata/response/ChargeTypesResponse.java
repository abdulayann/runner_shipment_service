package com.dpw.runner.booking.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class ChargeTypesResponse {
    @JsonProperty("Services")
    private String services;
}
