package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class RoutingsRequestV2 implements IRunnerRequest {
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
    @JsonProperty("Pol")
    public String Pol;
    @JsonProperty("Pod")
    public String Pod;
    @JsonProperty("RouteLegId")
    public Long RouteLegId;
    @JsonProperty("RoutingStatus")
    public String RoutingStatus;
    @JsonProperty("TransitDays")
    public Long TransitDays;
    @JsonProperty("VesselName")
    public String VesselName;
    @JsonProperty("Voyage")
    public String Voyage;
    @JsonProperty("ShipmentGuid")
    public UUID ShipmentGuid;
    @JsonProperty("ConsolidationGuid")
    public UUID ConsolidationGuid;
    @JsonProperty("ByCarrier")
    public String ByCarrier;
    @JsonProperty("Guid")
    private UUID Guid;
}
