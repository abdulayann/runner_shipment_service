package com.dpw.runner.shipment.services.dto.v1.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@SuppressWarnings("java:S1948")
public class V1ShipmentCreationResponse implements IRunnerResponse {
    @JsonProperty("Entity")
    public Object entity;

    @JsonProperty("ShipmentId")
    public String shipmentId;

    @JsonProperty("EntityId")
    public String entityId;

    @JsonProperty("ShipmentGuid")
    public String shipmentGuid;
}
