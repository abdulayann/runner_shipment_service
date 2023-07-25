package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;


@Data
@Builder
@ApiModel("Hbl Generate Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblGenerateRequest implements IRunnerRequest {
    private Long shipmentId;
}