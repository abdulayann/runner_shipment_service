package com.dpw.runner.shipment.services.dto.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.Map;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MeasurementBasisResponse {
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal chargable;
    private String chargeableUnit;
    private Map<String, Long> containerData;
    private Long containerCount;
    private BigDecimal teuCount;
    private Integer packCount;
}
