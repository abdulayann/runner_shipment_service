package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Getter
@Setter
@Schema(description = "Request for transfer Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RequestForTransferRequest implements IRunnerRequest {
    private Long id;
    private String remarks;
}
