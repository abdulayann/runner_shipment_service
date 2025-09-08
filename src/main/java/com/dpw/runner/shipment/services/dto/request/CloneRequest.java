package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

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
    private CloneFlagsRequest flags;
}