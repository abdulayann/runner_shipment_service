package com.dpw.runner.shipment.services.service_bus.model;

import lombok.Data;

@Data
public class ContainerPayloadDetails {
    private String bookingRef;
    private ContainerBoomiUniversalJson container;
}
