package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Event {
    private String eventId;
    private String entityType;
    private String eventCode;
    private String shipmentNumber;
    private String bookingNumber;
    private String description;
    private String actual;
    private String source;
    private String branch;
    private String createdAt;
    private String direction;
}
