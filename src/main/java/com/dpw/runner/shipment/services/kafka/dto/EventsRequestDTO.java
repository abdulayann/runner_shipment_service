package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class EventsRequestDTO {

    private LocalDateTime actual;
    private String description;
    private Long entityId;
    private String entityType;
    private LocalDateTime estimated;
    private String eventEstimateUpdateReasons;
    private Long id;
    private Boolean isPublicTrackingEvent;
    private String latitude;
    private String longitude;
    private String eventCode;
    private String placeDescription;
    private String placeName;
    private String source;
    private Long tenantId;
    private String status;
    private Integer pieces;
    private Integer totalPieces;
    private BigDecimal weight;
    private BigDecimal totalWeight;
    private Boolean isPartial;
    private LocalDateTime receivedDate;
    private LocalDateTime scheduledDate;
    private String containerNumber;
    private String locationRole;
    private Long consolidationId;
    private String shipmentNumber;
    private String flightNumber;
    private String flightName;

    // identifiers
    private UUID shipmentGuid;
    private UUID consolidationGuid;
}
