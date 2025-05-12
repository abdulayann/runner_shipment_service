package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class ShipmentGridChangeV3Response extends ShipmentGridChangeResponse {
    private String summaryDGShipmentsCount;
    private String summaryAssignedContainerCount;
    private String summaryAssignedTEUs;

    private Integer summaryAssignedDgPacks;
    private Integer summaryDgPacksCount;


}