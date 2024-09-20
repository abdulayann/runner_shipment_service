package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class CarrierDetailModel implements IDocumentModel {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("ShippingLine")
    private String shippingLine;
    @JsonProperty("Vessel")
    private String vessel;
    @JsonProperty("Voyage")
    private String voyage;
    @JsonProperty("FlightNumber")
    private String flightNumber;
    @JsonProperty("AircraftType")
    private String aircraftType;
    @JsonProperty("AircraftRegistration")
    private String aircraftRegistration;
    @JsonProperty("TruckRefNumber")
    private String truckRefNumber;
    @JsonProperty("JourneyNumber")
    private String journeyNumber;
    @JsonProperty("JourneyRefNumber")
    private String journeyRefNumber;
    @JsonProperty("Origin")
    private String origin;
    @JsonProperty("Destination")
    private String destination;
    @JsonProperty("Eta")
    private LocalDateTime eta;
    @JsonProperty("Etd")
    private LocalDateTime etd;
    @JsonProperty("Ata")
    private LocalDateTime ata;
    @JsonProperty("Atd")
    private LocalDateTime atd;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("OriginPort")
    private String originPort;
    @JsonProperty("DestinationPort")
    private String destinationPort;
    @JsonProperty("VesselBerthingDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime vesselBerthingDate;
}
