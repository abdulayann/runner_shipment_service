package com.dpw.runner.booking.services.dto.request.npm;

import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DimensionRequest implements IRunnerRequest {
    private BigDecimal length;
    private BigDecimal width;
    private BigDecimal height;
    private String uom;
}
