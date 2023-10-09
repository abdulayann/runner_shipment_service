package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class RoutingsRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("AircraftRegistration")
    public String AircraftRegistration;
    @JsonProperty("AircraftType")
    public String AircraftType;
    @JsonProperty("ETA")
    public LocalDateTime ETA;
    @JsonProperty("ATA")
    public LocalDateTime ATA;
    @JsonProperty("ETD")
    public LocalDateTime ETD;
    @JsonProperty("ATD")
    public LocalDateTime ATD;
    @JsonProperty("ConsolidationId")
    public Long ConsolidationId;
    @JsonProperty("isDomestic")
    public Boolean isDomestic;
    @JsonProperty("EntityType")
    public String EntityType;
    @JsonProperty("isLinked")
    public Boolean isLinked;
    @JsonProperty("FlightNumber")
    public String FlightNumber;
    @JsonProperty("Leg")
    public Long Leg;
    @JsonProperty("Mode")
    public String Mode;
    @JsonProperty("PolId")
    public Integer PolId;
    @JsonProperty("PodId")
    public Integer PodId;
    @JsonProperty("RouteLegId")
    public Long RouteLegId;
    @JsonProperty("RoutingStatus")
    public String RoutingStatus;
    @JsonProperty("ShipmentId")
    public Long ShipmentId;
    @JsonProperty("TransitDays")
    public Long TransitDays;
    @JsonProperty("VesselId")
    public Long VesselId;
    @JsonProperty("VesselName")
    public String VesselName;
    @JsonProperty("Voyage")
    public String Voyage;
    @JsonProperty("ShipmentGuid")
    public UUID ShipmentGuid;
    @JsonProperty("ConsolidationGuid")
    public UUID ConsolidationGuid;
//    public Long EntityId;
}
