package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class CargoChargeableRequest {
    private String transportMode;
    private String weightUnit;
    private String volumeUnit;
    private BigDecimal weight;
    private BigDecimal volume;
}
