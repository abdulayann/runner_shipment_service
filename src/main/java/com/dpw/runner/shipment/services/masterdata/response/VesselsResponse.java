package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.UUID;

@Data
public class VesselsResponse implements Serializable {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("Name")
    private String name;
    @JsonProperty("Guid")
    private UUID guid;
}
