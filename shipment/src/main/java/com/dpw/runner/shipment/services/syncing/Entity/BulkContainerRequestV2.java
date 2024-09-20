package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class BulkContainerRequestV2 implements IRunnerRequest {
    @JsonProperty("BulkContainers")
    private List<ContainerRequestV2> bulkContainers;
    @JsonProperty("ConsolidationId")
    private Long ConsolidationId;
    @JsonProperty("ShipmentId")
    private Long ShipmentId;
}
