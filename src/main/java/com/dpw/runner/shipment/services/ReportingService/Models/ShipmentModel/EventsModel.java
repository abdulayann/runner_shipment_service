package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class EventsModel {
    @JsonProperty("Actual")
    private LocalDateTime actual;
    @JsonProperty("Description")
    private String description;
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("EntityType")
    private String entityType;
    @JsonProperty("Estimated")
    private LocalDateTime estimated;
    @JsonProperty("Event_estimate_update_reasons")
    private String event_estimate_update_reasons;
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("IsPublicTrackingEvent")
    private Boolean isPublicTrackingEvent;
    @JsonProperty("Latitude")
    private String latitude;
    @JsonProperty("Longitude")
    private String longitude;
    @JsonProperty("EventCode")
    private String eventCode;
    @JsonProperty("PlaceDescription")
    private String placeDescription;
    @JsonProperty("PlaceName")
    private String placeName;
    @JsonProperty("Source")
    private String source;
    @JsonProperty("TenantId")
    private Long tenantId;
}
