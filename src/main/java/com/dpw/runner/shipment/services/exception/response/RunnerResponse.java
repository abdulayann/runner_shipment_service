package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.annotations.SchemaProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * /** * Generic response.
 *
 */
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor @Generated
@Schema(description= "Runner Response Model")
public class RunnerResponse {
    @SchemaProperty(position = 1, name = "success")
    private boolean success;

    @SchemaProperty(position = 6, name = "error")
    private ApiError error;

}

