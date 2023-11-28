package com.dpw.runner.shipment.services.dto.ContainerAPIsRequest;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.response.AchievedQuantitiesResponse;
import com.dpw.runner.shipment.services.dto.response.AllocationsResponse;
import lombok.Data;

@Data
public class ShipmentGridChangeResponse implements IRunnerResponse {
    private AchievedQuantitiesResponse achievedQuantities;
    private AllocationsResponse allocations;
    private String summaryWeight;
    private String summaryVolume;
    private Integer summaryShipmentsCount;
}