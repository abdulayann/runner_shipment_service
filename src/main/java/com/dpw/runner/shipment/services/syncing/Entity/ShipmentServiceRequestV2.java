package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import org.apache.tomcat.jni.Local;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

@Data
public class ShipmentServiceRequestV2 {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("ShipmentId")
    private Long ShipmentId;
    @JsonProperty("ConsolidationId")
    private Long ConsolidationId;
    @JsonProperty("ServiceType")
    private String ServiceType;
    @JsonProperty("Contractor")
    private PartyRequestV2 Contractor;
    @JsonProperty("SrvLocation")
    private String SrvLocation;
    @JsonProperty("BookingDate")
    private LocalDateTime BookingDate;
    @JsonProperty("ServiceCount")
    private Long ServiceCount;
    @JsonProperty("CompletionDate")
    private LocalDateTime CompletionDate;
    @JsonProperty("ServiceNotes")
    private String ServiceNotes;
    @JsonProperty("RefNumber")
    private String RefNumber;
    @JsonProperty("ServiceDurationSpan")
    private LocalTime ServiceDurationSpan;
}
