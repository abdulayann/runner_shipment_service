package com.dpw.runner.booking.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.io.Serializable;

@Data
public class SalesAgentResponse implements Serializable {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("SalesAgentName")
    private String salesAgentName;
}
