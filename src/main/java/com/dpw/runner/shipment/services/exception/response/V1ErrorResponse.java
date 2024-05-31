package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModel;
import lombok.*;

@SuppressWarnings("unused")
@ApiModel(description = "V1 Error Model")
@Builder @Data @AllArgsConstructor
@NoArgsConstructor @Getter @Generated
public class V1ErrorResponse {
    @JsonProperty("Error")
    private V1Error error;

    @Builder @Data @AllArgsConstructor
    @NoArgsConstructor @Getter
    public static class V1Error {
        @JsonProperty("Message")
        private String message;
    }

}

