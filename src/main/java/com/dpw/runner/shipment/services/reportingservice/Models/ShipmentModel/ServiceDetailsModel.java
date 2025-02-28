package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.time.LocalTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ServiceDetailsModel implements IDocumentModel {
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
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime bookingDate;
    @JsonProperty("ServiceCount")
    private Long serviceCount;
    @JsonProperty("ServiceDuration")
    private LocalTime serviceDuration;
    @JsonProperty("CompletionDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime completionDate;
    @JsonProperty("RefNumber")
    private String refNumber;
    @JsonProperty("ServiceNotes")
    private String serviceNotes;
}
