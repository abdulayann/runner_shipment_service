package com.dpw.runner.shipment.services.kafka.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BaseError {

    @JsonProperty("code")
    private String code;

    @JsonProperty("description")
    private String description;
}