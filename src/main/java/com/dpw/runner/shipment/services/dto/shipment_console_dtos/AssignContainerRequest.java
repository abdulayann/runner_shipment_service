package com.dpw.runner.shipment.services.dto.shipment_console_dtos;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class AssignContainerRequest implements IRunnerRequest {
    private Map<Long, List<Long>> shipmentPackIds; // Map<shipmentId, List<packingId>>
    private Long containerId;

    // Adding support for multiple OldContainers in case of ReAssign Request
    // oldContainerPackIds = Map<oldContainerId, List<packIds>>
    private Map<Long, List<Long>> oldContainerPackIds;
    private Boolean allowPackageReassignment;

    private Boolean allowCargoDetachIfRequired = false;
}
