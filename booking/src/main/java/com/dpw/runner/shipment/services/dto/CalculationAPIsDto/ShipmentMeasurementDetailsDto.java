package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Builder
@Data
@NoArgsConstructor
@AllArgsConstructor
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
