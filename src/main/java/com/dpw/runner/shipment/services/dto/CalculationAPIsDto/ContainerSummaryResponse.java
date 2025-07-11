package com.dpw.runner.shipment.services.dto.CalculationAPIsDto;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.List;

@Data
public class ContainerSummaryResponse implements IRunnerResponse {
    private String totalPackages;
    private String totalContainers;
    private String totalWeight; // gross weight
    private String totalTareWeight;
    private String chargeableWeight;
    private String totalContainerVolume;
    private String summary;
    private Integer dgContainers;
    private Integer totalDgPackages;
    private String totalNetWeight; // cargo weight
    private String assignedContainersCount;
    private List<GroupedContainerSummary> groupedContainersSummary;

    @Data
    public static class GroupedContainerSummary {
        private String containerCount;
        private String containerCode;
        private double totalWeight;
        private String totalWeightUnit;
        private double totalNetWeight;
        private String totalNetWeightUnit;
    }
}
