package com.dpw.runner.shipment.services.dto.v1.response;

import com.dpw.runner.shipment.services.syncing.Entity.ContainerRequestV2;
import com.fasterxml.jackson.annotation.JsonProperty;

public class TIContainerResponse extends ContainerRequestV2 {
    @JsonProperty("Id")
    private Long Id;
}
