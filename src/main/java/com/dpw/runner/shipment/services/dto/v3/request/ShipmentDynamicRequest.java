package com.dpw.runner.shipment.services.dto.v3.request;

import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class ShipmentDynamicRequest {
    private Long id;
    private UUID guid;
}
