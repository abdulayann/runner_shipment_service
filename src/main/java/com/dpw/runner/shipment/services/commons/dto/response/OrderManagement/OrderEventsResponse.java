package com.dpw.runner.shipment.services.commons.dto.response.OrderManagement;

import lombok.Data;

import java.io.Serializable;

@Data
public class OrderEventsResponse implements Serializable {
    private String eventId;
    private String eventCode;
    private String eventDescription;
    private String estimated;
    private String actual;
    private String description;
    private String placeName;
    private String latitude;
    private String longitude;
    private Boolean isPublicTrackingEvent;
    private String source;
    private String estimatedTime;
    private String actualTime;
}
