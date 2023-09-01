package com.dpw.runner.shipment.services.syncing.Entity;

import java.time.LocalDateTime;

public class EventsRequestV2 {

    public LocalDateTime Actual;
    public String Description;
    public Long EntityID;
    public String EntityType;
    public LocalDateTime Estimated;
    public String Event_estimate_update_reasons;
    public Boolean IsPublicTrackingEvent;
    public String Latitude;
    public String Longitude;
    public String EventCode;
    public String MasterList; // Not Present in Events Row
    public String PlaceDescription;
    public String PlaceName;
    public String Source;
    // public Int32 TenantId;
}
