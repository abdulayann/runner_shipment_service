package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;


@Data
@Builder
@Schema(description = "Hbl Generate Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblGenerateRequest implements IRunnerRequest {
    private Long shipmentId;
}