package com.dpw.runner.shipment.services.commons.dto.TrackingService;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class TrackingServiceApiRequest {
    private String shipmentReference;
}
