package com.dpw.runner.shipment.services.dto.v1.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class CreateShipmentTaskFromBookingTaskRequest implements IRunnerRequest {
    private String currentEntityType;
    private String currentEntityUuid;
    private String newEntityType;
    private String newEntityUuid;
}
