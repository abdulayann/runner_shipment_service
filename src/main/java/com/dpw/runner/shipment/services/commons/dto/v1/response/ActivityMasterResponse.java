package com.dpw.runner.shipment.services.commons.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.io.Serializable;

@Data
public class ActivityMasterResponse implements Serializable {
    @JsonProperty("ActivityCode")
    private String activityCode;
    @JsonProperty("ActivityName")
    private String activityName;
}
