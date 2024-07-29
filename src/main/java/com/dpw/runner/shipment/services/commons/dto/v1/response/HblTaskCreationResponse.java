package com.dpw.runner.shipment.services.commons.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class HblTaskCreationResponse {
    @JsonProperty("Success")
    private Boolean isCreated;
}
