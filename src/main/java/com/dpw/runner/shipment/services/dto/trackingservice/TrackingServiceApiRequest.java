package com.dpw.runner.shipment.services.dto.trackingservice;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class TrackingServiceApiRequest {
    private String shipmentReference;
}
