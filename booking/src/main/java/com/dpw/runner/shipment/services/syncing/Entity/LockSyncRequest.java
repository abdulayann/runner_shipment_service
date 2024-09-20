package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class LockSyncRequest {

    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("LockStatus")
    private boolean lockStatus;

}
