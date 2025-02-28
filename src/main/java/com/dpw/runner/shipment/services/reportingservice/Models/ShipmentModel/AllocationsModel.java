package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class AllocationsModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("ShipmentsCount")
    private Integer shipmentsCount;
    @JsonProperty("CutoffDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
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
