package com.dpw.runner.shipment.services.syncing.Entity;

import java.time.LocalDateTime;

public class RoutingsRequestV2 {

    public String AircraftRegistration;
    public String AircraftType;
    public LocalDateTime ETA;
    public LocalDateTime ATA;
    public LocalDateTime ETD;
    public LocalDateTime ATD;
    public Long ConsolidationId;
    public Boolean isDomestic;
    public String EntityType;
    public Boolean isLinked;
    public String FlightNumber;
    public Long Leg;
    public String Mode;
    public Integer PolId;
    public Integer PodId;
    public Long RouteLegId;
    public String RoutingStatus;
    public Long ShipmentId;
    public Long TransitDays;
    public Long VesselId;
    public String VesselName;
    public String Voyage;
    public Long EntityId;
}
