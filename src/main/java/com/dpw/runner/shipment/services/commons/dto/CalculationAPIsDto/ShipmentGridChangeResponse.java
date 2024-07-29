package com.dpw.runner.shipment.services.commons.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.commons.dto.response.AllocationsResponse;
import lombok.Data;

@Data
public class ShipmentGridChangeResponse implements IRunnerResponse {
    private AchievedQuantitiesResponse achievedQuantities;
    private AllocationsResponse allocations;
    private String summaryWeight;
    private String summaryVolume;
    private Integer summaryShipmentsCount;
    private String summaryConsolContainer;
    private String summaryShipmentContainer;
    private String summaryConsoleTEU;
    private String summaryShipmentTEU;
    private String summaryChargeableWeight;
}