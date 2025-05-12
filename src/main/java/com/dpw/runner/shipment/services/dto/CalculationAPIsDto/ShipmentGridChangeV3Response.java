package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import lombok.Data;
import lombok.EqualsAndHashCode;

@EqualsAndHashCode(callSuper = true)
@Data
public class ShipmentGridChangeV3Response extends ShipmentGridChangeResponse {
    private String summaryDGShipments;
    private String summaryContainer;
    private String summaryAssignedTEUs;
    private String summaryDgPacks;
    private Integer totalPacks;



}