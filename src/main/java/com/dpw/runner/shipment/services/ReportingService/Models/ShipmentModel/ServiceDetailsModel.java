package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.time.LocalTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ServiceDetailsModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("ServiceType")
    private String serviceType;
    @JsonProperty("Contractor")
    private PartiesModel contractor;
    @JsonProperty("SrvLocation")
    private String srvLocation;
    @JsonProperty("BookingDate")
    private LocalDateTime bookingDate;
    @JsonProperty("ServiceCount")
    private Long serviceCount;
    @JsonProperty("ServiceDuration")
    private LocalTime serviceDuration;
    @JsonProperty("CompletionDate")
    private LocalDateTime completionDate;
    @JsonProperty("RefNumber")
    private String refNumber;
    @JsonProperty("ServiceNotes")
    private String serviceNotes;
}
