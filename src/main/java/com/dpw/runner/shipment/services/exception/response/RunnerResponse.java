package com.dpw.runner.shipment.services.exception.response;

import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.media.SchemaProperty;
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
@JsonPropertyOrder({"success", "error"})
public class RunnerResponse {
    @SchemaProperty(name = "success")
    private boolean success;

    @SchemaProperty(name = "error")
    private ApiError error;

}

