package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.time.LocalDateTime;

@Data
public class AwbRoutingInfoV2 {
    // public Int64 entityId;
    public String entityType;
    //TODO- LocCode
    public String originString;
    public String destinationString;
    public String  byCarrier;
    public String  flightNumber;
    public LocalDateTime flightDate;
    public String departureAirport;
    public String destinationAirport;
    public String destinationPortName;
    public String originPortName;
}
