package com.dpw.runner.shipment.services.dto.response;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
@ApiModel("Cargo Details Response Model")
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
    private Integer containers;
    private BigDecimal teuCount;
}
