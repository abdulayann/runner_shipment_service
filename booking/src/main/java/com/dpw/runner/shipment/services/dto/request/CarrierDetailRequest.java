package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;


@Data
@Builder
@ApiModel("Carrier Details Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CarrierDetailRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private String shippingLine;
    private String vessel;
    private String voyage;
    private String flightNumber;
    private String aircraftType;
    private String aircraftRegistration;
    private String truckRefNumber;
    private String journeyNumber;
    private String journeyRefNumber;
    private String origin;
    private String destination;
    @ExcludeTimeZone
    private LocalDateTime eta;
    @ExcludeTimeZone
    private LocalDateTime etd;
    @ExcludeTimeZone
    private LocalDateTime ata;
    @ExcludeTimeZone
    private LocalDateTime atd;
    private Long shipmentId;
    private String originPort;
    private String destinationPort;
    private String cfs;
    private LocalDateTime vesselBerthingDate;
    private String carrierCountry;
    private String minTransitHours;
    private String maxTransitHours;
    private Boolean carrierAddedFromNpm;
    private Boolean isCarrierChanged;
    private String originLocCode;
    private String destinationLocCode;
    private String originPortLocCode;
    private String destinationPortLocCode;
}