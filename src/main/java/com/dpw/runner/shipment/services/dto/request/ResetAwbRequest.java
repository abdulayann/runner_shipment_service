package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.AwbReset;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Data
@Schema(description = "Reset Awb Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ResetAwbRequest implements IRunnerRequest {
    private Long id;
    private Long shipmentId;
    private Long consolidationId;
    private AwbReset resetType;
    private String awbType;
}
