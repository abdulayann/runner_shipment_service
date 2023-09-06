package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;

@Data
@Builder
@ApiModel("AWB Routing Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AwbRoutingInfo {
    private Long entityId;
    private String entityType;
    //TODO- LocCode
    private String origin;
    private String destination;
    private String  byCarrier;
    private String  flightNumber;
    private LocalDateTime flightDate;
    private String departureAirport;
    private String destinationAirport;
    private String destinationPortName;
    private String originPortName;
}
