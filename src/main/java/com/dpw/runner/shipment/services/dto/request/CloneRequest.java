package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * Request model to clone.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ApiModel("Clone Request")
public class CloneRequest extends CommonRequest implements IRunnerRequest {
    private Long shipmentId;
    private Long bookingId;
    @NotNull(message = "Flags cannot be null")
    @Valid
    @ApiModelProperty(value = "Clone configuration flags", required = true)
    private CloneFlagsRequest flags;
}