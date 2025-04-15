package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TrackingEventsRequest extends ListCommonRequest {
    private Long shipmentId;
    private Long consolidationId;
    private String shipmentNumber;
}
