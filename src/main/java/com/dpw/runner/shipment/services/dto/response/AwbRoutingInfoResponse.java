package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.Map;

@Data
public class AwbRoutingInfoResponse implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private String origin;
    private String destination;
    private String  byCarrier;
    private String  flightNumber;
    private LocalDateTime flightDate;
    private String departureAirport;
    private String destinationAirport;
    @UnlocationData
    private String destinationPortName;
    @UnlocationData
    private String originPortName;
    private Boolean isShipmentCreated;

    //Master Data
    public Map<String, String> masterData;
    public Map<String, String> unlocationData;
}
