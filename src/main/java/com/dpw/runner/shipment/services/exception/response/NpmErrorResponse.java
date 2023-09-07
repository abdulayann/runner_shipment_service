package com.dpw.runner.shipment.services.exception.response;

import io.swagger.annotations.ApiModel;
import lombok.*;

@SuppressWarnings("unused")
@ApiModel(description = "NPM Error Model")
@Builder @Data @AllArgsConstructor
@NoArgsConstructor @Getter
public class NpmErrorResponse {
    private Boolean success;
    private String errorMessage;
}
