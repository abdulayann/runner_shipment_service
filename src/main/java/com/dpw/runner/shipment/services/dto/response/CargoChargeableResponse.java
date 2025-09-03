package com.dpw.runner.shipment.services.dto.response;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
public class CargoChargeableResponse {
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal chargeable;
    private String chargeableUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
}
