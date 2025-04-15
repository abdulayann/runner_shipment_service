package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
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
public class EventsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long entityId;
    private String entityType;
    private String eventCode;
    private String eventType;
    private Long shipmentId;
    private String shipmentNumber;
    private Long consolidationId;
    private String description;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime estimated;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime actual;
    private Boolean isPublicTrackingEvent;
    private String tenantId;
    private String placeName;
    private String placeDescription;
    private String latitude;
    private String longitude;
    private String source;
    private String event_estimate_update_reasons;
    private String flightNumber;
    private String flightName;
    private String remarks;
    private String userName;
    private String userEmail;
    private String branch;
    private String branchName;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime createdAt; // Generated Date field for UI

    // Conditional response from TrackingEvents
    private String containerNumber;
    private String locationRole;
    private String awbNumber;
    private String status;
    private Integer pieces;
    private Integer totalPieces;
    private BigDecimal weight;
    private BigDecimal totalWeight;
    private Boolean isPartial;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime receivedDate;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime scheduledDate;
    private String direction;
    private String referenceNumber;
}
