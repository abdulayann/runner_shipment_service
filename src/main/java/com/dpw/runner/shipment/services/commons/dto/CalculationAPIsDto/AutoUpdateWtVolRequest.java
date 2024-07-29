package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.dto.request.ContainerRequest;
import com.dpw.runner.shipment.services.commons.dto.request.PackingRequest;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
public class AutoUpdateWtVolRequest implements IRunnerRequest {
    private List<ContainerRequest> containersList;
    private List<PackingRequest> packingList;
    private String transportMode;
    private String shipmentType;
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
    private ContainerSummaryRequest containerSummary;
    private PackSummaryRequest packSummary;
}
