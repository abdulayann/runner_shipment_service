package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.UUID;

@Data
public class ShipmentServiceRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
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
