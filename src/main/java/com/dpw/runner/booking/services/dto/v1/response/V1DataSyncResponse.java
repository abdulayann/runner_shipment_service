package com.dpw.runner.booking.services.dto.v1.response;

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

    @JsonProperty("IsSuccess")
    private Boolean isSuccess;

    @JsonProperty("Error")
    public Object error;
}