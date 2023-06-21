package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.Date;

@Data
public class EventsResponse implements IRunnerResponse {
    private Long id;
    private String guid;
    private String masterList;
    private Long shipmentId;
    private String description;
    private Date estimated;
    private Date actual;
    private Boolean isPublicTrackingEvent;
    private String tenantId;
    private String placeName;
    private String placeDescription;
    private String latitude;
    private String longitude;
    private String source;
    private String event_estimate_update_reasons;
}
