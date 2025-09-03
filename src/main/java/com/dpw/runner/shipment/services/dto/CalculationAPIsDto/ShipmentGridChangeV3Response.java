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
    private String packType;
    private String shipmentSummaryChargeableSum; // chargeable sum of shipment values
    private String shipmentSummaryWeightVolumeSum; // weight volume sum of shipment values



}