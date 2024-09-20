package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class EventsRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("EntityType")
    private String EntityType;
    @JsonProperty("Actual")
    private LocalDateTime Actual;
    @JsonProperty("Description")
    private String Description;
    @JsonProperty("Estimated")
    private LocalDateTime Estimated;
    @JsonProperty("Event_estimate_update_reasons")
    private String Event_estimate_update_reasons;
    @JsonProperty("IsPublicTrackingEvent")
    private Boolean IsPublicTrackingEvent;
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
    @JsonProperty("ShipmentGuid")
    private UUID ShipmentGuid;
    @JsonProperty("ConsolidationGuid")
    private UUID ConsolidationGuid;

    // Tracking events conditional fields
    @JsonProperty("ContainerNumber")
    private String containerNumber;
    @JsonProperty("AWBNumber")
    private String awbNumber;
}
