package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class ShipmentMeasurementDetailsDto {
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private String noOfPacks;
    private String packsUnit;
    private Integer innerPacks;
    private String innerPackUnit;
}
