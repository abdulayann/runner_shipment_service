package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class NoteRequestV2 {
    @JsonProperty("InsertUserDisplayName")
    private String InsertUserDisplayName;
    @JsonProperty("IsPublic")
    private Boolean IsPublic;
    @JsonProperty("Text")
    private String Text;
}
