package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;

@Setter
@Getter
@ApiModel("Container Details Response Model")
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContainerDetailsResponse implements IRunnerResponse {
    private BigDecimal teuCount;
    private Long containers;
}
