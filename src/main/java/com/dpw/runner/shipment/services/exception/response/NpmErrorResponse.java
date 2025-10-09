package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@SuppressWarnings("unused")
@Schema(description = "NPM Error Model")
@Builder @Data @AllArgsConstructor
@NoArgsConstructor @Getter @Generated
public class NpmErrorResponse {
    private Boolean success;
    private String errorMessage;
}
