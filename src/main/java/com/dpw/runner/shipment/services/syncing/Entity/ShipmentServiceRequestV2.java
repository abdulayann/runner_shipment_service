package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class ShipmentServiceRequestV2 {
    private Long ShipmentId;
    private Long ConsolidationId;
    private String ServiceType;
    private PartyRequestV2 Contractor;
    private Long SrvLocation;
    private LocalDateTime BookingDate;
    private Long ServiceCount;
    private LocalDateTime CompletionDate;
    private String ServiceNotes;
    private String RefNumber;
    private Object ServiceDurationSpan;
}
