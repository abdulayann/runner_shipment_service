package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.syncing.Entity.EventsRequestV2;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TrackingEventsResponse {
    @JsonProperty("Events")
    private List<EventsRequestV2> Events;
    @JsonProperty("ShipmentAta")
    private LocalDateTime ShipmentAta;
    @JsonProperty("ShipmentAtd")
    private LocalDateTime ShipmentAtd;

    private List<com.dpw.runner.shipment.services.entity.Events> eventsList;
}
