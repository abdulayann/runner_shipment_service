package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class V1ContainerTypeResponse {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("Code")
    private String code;
    @JsonProperty("Description")
    private String description;
    @JsonProperty("Mode")
    private String mode;
    @JsonProperty("ContainerType")
    private String containerType;
    @JsonProperty("Teu")
    private BigDecimal teu;
}
