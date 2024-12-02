package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Request for transfer Model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RequestForTransferRequest implements IRunnerRequest {
    private Long id;
    private String remarks;
}
