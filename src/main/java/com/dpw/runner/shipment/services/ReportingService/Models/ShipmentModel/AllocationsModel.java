package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class AllocationsModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("ShipmentsCount")
    private Integer shipmentsCount;
    @JsonProperty("Hazardous")
    private Boolean hazardous;
    @JsonProperty("CutoffDate")
    private LocalDateTime cutoffDate;
    @JsonProperty("IsTemperatureControlled")
    private Boolean isTemperatureControlled;
    @JsonProperty("Weight")
    private BigDecimal weight;
    @JsonProperty("WeightUnit")
    private String weightUnit;
    @JsonProperty("Volume")
    private BigDecimal volume;
    @JsonProperty("VolumeUnit")
    private String volumeUnit;
    @JsonProperty("Chargable")
    private BigDecimal chargable;
    @JsonProperty("ChargeableUnit")
    private String chargeableUnit;
    @JsonProperty("MinTemp")
    private BigDecimal minTemp;
    @JsonProperty("MinTempUnit")
    private String minTempUnit;
    @JsonProperty("MaxTemp")
    private BigDecimal maxTemp;
    @JsonProperty("MaxTempUnit")
    private String maxTempUnit;
}
