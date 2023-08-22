package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class DimensionDTO {
    private BigDecimal length;
    private BigDecimal width;
    private BigDecimal height;
    private String uom;
}
