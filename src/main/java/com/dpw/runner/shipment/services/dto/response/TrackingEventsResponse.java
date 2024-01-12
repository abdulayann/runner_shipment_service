package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Data
public class TrackingEventsResponse {
    @JsonProperty("Events")
    private List<EventsRequestV2> Events;
    @JsonProperty("ShipmentAta")
    private LocalDateTime ShipmentAta;
    @JsonProperty("ShipmentAtd")
    private LocalDateTime ShipmentAtd;
}
