package com.dpw.runner.shipment.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class V1DataSyncResponse {

    @JsonProperty("Error")
    public Object error;
    @JsonProperty("IsSuccess")
    private Boolean isSuccess;
}