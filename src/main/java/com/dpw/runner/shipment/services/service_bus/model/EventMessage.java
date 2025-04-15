package com.dpw.runner.shipment.services.service_bus.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Builder
@Data
@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class EventMessage {
    @JsonProperty("MessageType")
    private String messageType;

    @JsonProperty("Entity")
    private String entity;

    @JsonProperty("Request")
    private Object request;

    @JsonProperty("ContainerUpdateRequest")
    private ContainerUpdateRequest containerUpdateRequest;
}
