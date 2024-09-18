package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.enums.StatusType;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class ShipmentStatusPayload {
    @JsonProperty("guid")
    private String guid;

    @JsonProperty("status")
    private StatusType status;

    @JsonProperty("errorMessage")
    private String errorMessage;

    @JsonProperty("messageType")
    private String messageType;

    @JsonProperty("xmlPayload")
    private String xmlPayload;
}
