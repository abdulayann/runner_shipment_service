package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;

@Data
public class EventsRequestV2 {

    @JsonProperty("Actual")
    private LocalDateTime Actual;
    @JsonProperty("Description")
    private String Description;
    @JsonProperty("Estimated")
    private LocalDateTime Estimated;
    @JsonProperty("Event_estimate_update_reasons")
    private String Event_estimate_update_reasons;
    @JsonProperty("IsprivateTrackingEvent")
    private Boolean IsprivateTrackingEvent;
    @JsonProperty("Latitude")
    private String Latitude;
    @JsonProperty("Longitude")
    private String Longitude;
    @JsonProperty("EventCode")
    private String EventCode;
    @JsonProperty("MasterList")
    private String MasterList; // Not Present in Events Row
    @JsonProperty("PlaceDescription")
    private String PlaceDescription;
    @JsonProperty("PlaceName")
    private String PlaceName;
    @JsonProperty("Source")
    private String Source;
}
