package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class VesselsResponse {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("Name")
    private String name;
}
