package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;
import java.io.Serializable;
import java.time.LocalDateTime;

@Data
@Builder
@ApiModel("AWB Routing Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbRoutingInfo implements Serializable {
    private Long leg;
    private Long entityId;
    private String entityType;
    private String origin;
    private String destination;
    private String  byCarrier;
    private String  flightNumber;
    @ExcludeTimeZone
    private LocalDateTime flightDate;
    @ExcludeTimeZone
    private LocalDateTime eta;
    private String departureAirport;
    private String destinationAirport;
    @UnlocationData
    private String destinationPortName;
    @UnlocationData
    private String originPortName;
    private Boolean isShipmentCreated;
}
