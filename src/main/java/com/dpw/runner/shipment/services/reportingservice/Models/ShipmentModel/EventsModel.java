package com.dpw.runner.shipment.services.reportingservice.Models.ShipmentModel;

import com.dpw.runner.shipment.services.reportingservice.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class EventsModel implements IDocumentModel {
    @JsonProperty("Actual")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime actual;
    @JsonProperty("Description")
    private String description;
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("EntityType")
    private String entityType;
    @JsonProperty("Estimated")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
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
    @JsonProperty("ContainerNumber")
    private String containerNumber;
    @JsonProperty("LocationRole")
    private String locationRole;
}
