package com.dpw.runner.shipment.services.dto.ContainerAPIsRequest;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;

@Data
public class ShipmentContainerAssignRequest implements IRunnerRequest {
    private Long shipmentId;
    private List<Long> containerIds;
}
