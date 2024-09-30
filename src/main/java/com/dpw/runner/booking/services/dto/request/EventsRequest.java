package com.dpw.runner.booking.services.dto.request;

import com.dpw.runner.booking.services.commons.requests.CommonRequest;
import com.dpw.runner.booking.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class EventsRequest extends CommonRequest implements IRunnerRequest {
    private LocalDateTime actual;
    private String description;
    private Long entityId;
    private String entityType;
    private LocalDateTime estimated;
    private String event_estimate_update_reasons;
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
}
