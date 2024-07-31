package com.dpw.runner.shipment.services.commons.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.entity.enums.AwbReset;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@ApiModel("Reset Awb Model")
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
