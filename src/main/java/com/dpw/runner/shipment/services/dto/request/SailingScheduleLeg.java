package com.dpw.runner.shipment.services.dto.request;

import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@ApiModel("Sailing Schedule Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class SailingScheduleLeg {
    private Long id;
    private Integer sequence;
    private String transportId;
    private String serviceName;
    private String transportType;
    private String transportName;
    private String transportDisplayName;
    private String vesselGuid;
    private String conveyanceNumber;
    private String departureUnlocReferenceGuid;
    private String departureUnloc;
    private String departureCityName;
    private String departureCountry;
    private String departureSubdivision;
    private String departureTerminal;
    private LocalDateTime estimatedDepartureDate;
    private LocalDateTime actualDepartureDate;
    private String arrivalUnlocReferenceGuid;
    private String arrivalUnloc;
    private String arrivalCityName;
    private String arrivalCountry;
    private String arrivalSubdivision;
    private String arrivalTerminal;
    private LocalDateTime estimatedArrivalDate;
    private LocalDateTime actualArrivalDate;
    private Boolean transshipmentIndicator;
    private Integer transitDuration;
    private LocalDateTime insertDate;
    private LocalDateTime updateDate;
    private String guid;
}
