package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class AutoUpdateWtVolResponse implements IRunnerResponse {
    private String noOfPacks;
    private String packsUnit;
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal chargable;
    private String chargeableUnit;
    private BigDecimal volumetricWeight;
    private String volumetricWeightUnit;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private Integer innerPacks;
    private String innerPackUnit;
    private ContainerSummaryResponse containerSummary;
    private PackSummaryResponse packSummary;
}
