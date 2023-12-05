package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class RoutingsModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("BookingId")
    private Long bookingId;
    @JsonProperty("Leg")
    private Long leg;
    @JsonProperty("Mode")
    private String mode;
    @JsonProperty("RoutingStatus")
    private String routingStatus;
    @JsonProperty("VesselName")
    private String vesselName;
    @JsonProperty("Pol")
    private String pol;
    @JsonProperty("Pod")
    private String pod;
    @JsonProperty("IsDomestic")
    private boolean isDomestic;
    @JsonProperty("Eta")
    private LocalDateTime eta;
    @JsonProperty("Etd")
    private LocalDateTime etd;
    @JsonProperty("Ata")
    private LocalDateTime ata;
    @JsonProperty("Atd")
    private LocalDateTime atd;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("IsLinked")
    private Boolean isLinked;
    @JsonProperty("Voyage")
    private String voyage;
    @JsonProperty("AircraftRegistration")
    private String aircraftRegistration;
    @JsonProperty("FlightNumber")
    private String flightNumber;
    @JsonProperty("AircraftType")
    private String aircraftType;
    @JsonProperty("EntityType")
    private String entityType;
    @JsonProperty("EntityId")
    private Long entityId;
    @JsonProperty("RouteLegId")
    private Long routeLegId;
    @JsonProperty("VesselId")
    private Long vesselId;
    @JsonProperty("TransitDays")
    private Long transitDays;
    @JsonProperty("Carrier")
    private String carrier;
}
