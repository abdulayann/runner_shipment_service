package com.dpw.runner.shipment.services.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ShipmentEventUpdatePayload {

    @JsonProperty("guid")
    private String guid;

    @JsonProperty("estimatedTimeOfEvent")
    private LocalDateTime estimatedTimeOfEvent;

    @JsonProperty("actualTimeOfEvent")
    private LocalDateTime actualTimeOfEvent;

    @JsonProperty("scheduledTimeOfEvent")
    private LocalDateTime scheduledTimeOfEvent;

    @JsonProperty("eventCode")
    private String eventCode;

    @JsonProperty("placeOfEvent")
    private String placeOfEvent;

    @JsonProperty("placeDescription")
    private String placeDescription;

    @JsonProperty("longitude")
    private Double longitude;

    @JsonProperty("latitude")
    private Double latitude;

    @JsonProperty("sourceCarrier")
    private String sourceCarrier;

    @JsonProperty("actualTimeOfDeparture")
    private LocalDateTime actualTimeOfDeparture;

    @JsonProperty("scheduledTimeOfDeparture")
    private LocalDateTime scheduledTimeOfDeparture;

    @JsonProperty("estimatedTimeOfDeparture")
    private LocalDateTime estimatedTimeOfDeparture;

    @JsonProperty("actualTimeOfArrival")
    private LocalDateTime actualTimeOfArrival;

    @JsonProperty("scheduledTimeOfArrival")
    private LocalDateTime scheduledTimeOfArrival;

    @JsonProperty("estimatedTimeOfArrival")
    private LocalDateTime estimatedTimeOfArrival;

    @JsonProperty("timeOfReceivingFSUMessage")
    private LocalDateTime timeOfReceivingFSUMessage;

    @JsonProperty("timeOfEventSentByCarrier")
    private LocalDateTime timeOfEventSentByCarrier;

    @JsonProperty("origin")
    private String origin;

    @JsonProperty("destination")
    private String destination;

    @JsonProperty("pieces")
    private Integer pieces;

    @JsonProperty("totalPieces")
    private Integer totalPieces;

    @JsonProperty("weight")
    private Double weight;

    @JsonProperty("totalWeight")
    private Double totalWeight;

    @JsonProperty("isPartial")
    private String isPartial;

    @JsonProperty("comments")
    private String comments;

    @JsonProperty("xmlPayload")
    private String xmlPayload;
}
