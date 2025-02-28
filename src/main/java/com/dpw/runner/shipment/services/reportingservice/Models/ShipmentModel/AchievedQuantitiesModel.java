package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class AchievedQuantitiesModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("WeightVolume")
    private BigDecimal weightVolume;
    @JsonProperty("WeightVolumeUnit")
    private String weightVolumeUnit;
    @JsonProperty("ConsolidatedWeight")
    private BigDecimal consolidatedWeight;
    @JsonProperty("ConsolidatedWeightUnit")
    private String consolidatedWeightUnit;
    @JsonProperty("ConsolidatedVolume")
    private BigDecimal consolidatedVolume;
    @JsonProperty("ConsolidatedVolumeUnit")
    private String consolidatedVolumeUnit;
    @JsonProperty("ConsolidationChargeQuantity")
    private BigDecimal consolidationChargeQuantity;
    @JsonProperty("ConsolidationChargeQuantityUnit")
    private String consolidationChargeQuantityUnit;
    @JsonProperty("WeightUtilization")
    private String weightUtilization;
    @JsonProperty("VolumeUtilization")
    private String volumeUtilization;
}
