package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class BulkPackingRequestV2 implements IRunnerRequest {
    @JsonProperty("BulkPacking")
    private List<PackingRequestV2> bulkPacking;
    @JsonProperty("ConsolidationId")
    private Long ConsolidationId;
    @JsonProperty("ShipmentId")
    private Long ShipmentId;
}