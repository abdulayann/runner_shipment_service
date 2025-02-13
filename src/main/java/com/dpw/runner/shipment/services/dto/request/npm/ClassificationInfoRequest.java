package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ClassificationInfoRequest implements IRunnerRequest {
    private BigDecimal code;
    private String description;
}
