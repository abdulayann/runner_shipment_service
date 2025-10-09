package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;

/**
 * Request model to clone.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "Clone Request")
public class CloneRequest extends CommonRequest implements IRunnerRequest {
    private Long shipmentId;
    private Long bookingId;
    @NotNull(message = "Flags cannot be null")
    @Valid
    @Schema(description = "Clone configuration flags", required = true)
    private CloneFlagsRequest flags;
}