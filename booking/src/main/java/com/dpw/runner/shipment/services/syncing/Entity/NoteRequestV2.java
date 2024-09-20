package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.UUID;

@Data
public class NoteRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("InsertUserDisplayName")
    private String InsertUserDisplayName;
    @JsonProperty("IsPublic")
    private Boolean IsPublic;
    @JsonProperty("Text")
    private String Text;
}
