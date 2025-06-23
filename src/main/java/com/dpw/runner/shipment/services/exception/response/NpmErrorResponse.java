package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import io.swagger.annotations.ApiModel;
import lombok.*;

@SuppressWarnings("unused")
@ApiModel(description = "NPM Error Model")
@Builder @Data @AllArgsConstructor
@NoArgsConstructor @Getter @Generated
public class NpmErrorResponse {
    private Boolean success;
    private String errorMessage;
}
