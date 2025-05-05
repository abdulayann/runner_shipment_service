package com.dpw.runner.shipment.services.dto.response;

import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
public class CargoDetailsResponse {
    private String transportMode;
    private String shipmentType;
    private Integer noOfPacks;
    private String packsUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal chargable;
    private String chargeableUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    private BigDecimal weight;
    private String weightUnit;
}
