package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Data
@Schema("Create Awb Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateAwbRequest implements IRunnerRequest {
    private Long ShipmentId;
    private Long ConsolidationId;
    private String AwbType;
    private Boolean isReset;
}
