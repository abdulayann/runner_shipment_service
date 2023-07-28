package com.dpw.runner.shipment.services.service_bus.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.Setter;

@Builder
@Data
@Getter @Setter
public class EventMessage {
    @JsonProperty("MessageType")
    private String messageType;

    @JsonProperty("Entity")
    private String entity;

    @JsonProperty("Request")
    private Object request;
}
