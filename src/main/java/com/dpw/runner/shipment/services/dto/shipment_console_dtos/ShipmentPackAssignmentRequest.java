package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;

@Data
public class ShipmentPackAssignmentRequest implements IRunnerRequest {
    private Long containerId;
    private List<Long> packingIds;
}
