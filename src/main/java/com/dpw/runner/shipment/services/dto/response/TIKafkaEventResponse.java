package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.List;

@Data
public class TIKafkaEventResponse implements IRunnerResponse {
    private Long shipmentId;
    private List<IRunnerResponse> pickupDeliveryDetails;
}
