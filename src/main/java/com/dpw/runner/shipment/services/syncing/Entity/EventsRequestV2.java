package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.time.LocalDateTime;

public class EventsRequestV2 {

    @JsonProperty("Actual")
    public LocalDateTime Actual;
    @JsonProperty("Description")
    public String Description;
    @JsonProperty("Estimated")
    public LocalDateTime Estimated;
    @JsonProperty("Event_estimate_update_reasons")
    public String Event_estimate_update_reasons;
    @JsonProperty("IsPublicTrackingEvent")
    public Boolean IsPublicTrackingEvent;
    @JsonProperty("Latitude")
    public String Latitude;
    @JsonProperty("Longitude")
    public String Longitude;
    @JsonProperty("EventCode")
    public String EventCode;
    @JsonProperty("MasterList")
    public String MasterList; // Not Present in Events Row
    @JsonProperty("PlaceDescription")
    public String PlaceDescription;
    @JsonProperty("PlaceName")
    public String PlaceName;
    @JsonProperty("Source")
    public String Source;
//    public Long EntityID;
//    public String EntityType;
    // public Int32 TenantId;
}
