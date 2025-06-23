package com.dpw.runner.shipment.services.kafka.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class AirMessagingEventDto {
    private UUID guid;
    private LocalDateTime estimatedTimeOfEvent;
    private LocalDateTime actualTimeOfEvent;
    private LocalDateTime scheduledTimeOfEvent;
    private String eventCode;
    private String placeOfEvent;
    private String placeDescription;
    private String longitude;
    private String latitude;
    private String sourceCarrier;
    private LocalDateTime actualTimeOfDeparture;
    private LocalDateTime scheduledTimeOfDeparture;
    private LocalDateTime estimatedTimeOfDeparture;
    private LocalDateTime actualTimeOfArrival;
    private LocalDateTime scheduledTimeOfArrival;
    private LocalDateTime estimatedTimeOfArrival;
    private LocalDateTime timeOfReceivingFSUMessage;
    private LocalDateTime timeOfEventSentByCarrier;
    private String origin;
    private String destination;
    private Integer pieces;
    private Integer totalPieces;
    private BigDecimal weight;
    private BigDecimal totalWeight;
    private String isPartial;
    private String comments;
    private String xmlPayload;
}
