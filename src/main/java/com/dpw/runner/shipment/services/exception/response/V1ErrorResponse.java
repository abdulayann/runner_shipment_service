package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@SuppressWarnings("unused")
@Schema(description = "V1 Error Model")
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

