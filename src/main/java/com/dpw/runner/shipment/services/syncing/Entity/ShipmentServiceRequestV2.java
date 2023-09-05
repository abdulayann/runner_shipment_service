package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ShipmentServiceRequestV2 {
    @JsonProperty("ShipmentId")
    private Long ShipmentId;
    @JsonProperty("ConsolidationId")
    private Long ConsolidationId;
    @JsonProperty("ServiceType")
    private String ServiceType;
    @JsonProperty("Contractor")
    private PartyRequestV2 Contractor;
    @JsonProperty("SrvLocation")
    private Long SrvLocation;
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
    private Object ServiceDurationSpan;
}
