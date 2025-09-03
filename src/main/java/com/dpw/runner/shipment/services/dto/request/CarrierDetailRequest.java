package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.validation.constraints.Size;
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
    @Size(max=64, message = "max size is 64 for shipping line" )
    private String shippingLine;
    private String vessel;
    @Size(max = 20, message = "max size is 20 for voyage")
    private String voyage;
    private String flightNumber;
    @Size(max=20, message = "max size is 20 for aircraft type")
    private String aircraftType;
    @Size(max=20, message = "max size is 20 for aircraft registration")
    private String aircraftRegistration;
    private String truckRefNumber;
    @Size(max=10, message = "max size is 10 for journey number")
    private String journeyNumber;
    @Size(max=10, message = "max size is 10 for journey ref number")
    private String journeyRefNumber;
    @Size(max=100, message = "max size is 100 for origin")
    private String origin;
    @Size(max=100, message = "max size is 100 for destination")
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
    @Size(max=100, message = "max size is 100 for cfs")
    private String cfs;
    private LocalDateTime vesselBerthingDate;
    @Size(max = 32, message = "max size is 32 for carrier country")
    private String carrierCountry;
    @Size(max = 15, message = "max size is 15 for min transit hours")
    private String minTransitHours;
    @Size(max = 15, message = "max size is 15 for max transit hours")
    private String maxTransitHours;
    private Boolean carrierAddedFromNpm;
    private Boolean isCarrierChanged;
    @Size(max=64, message = "max size is 64 for origin loc code")
    private String originLocCode;
    @Size(max=64, message = "max size is 64 for destination loc code")
    private String destinationLocCode;
    @Size(max=64, message = "max size is 64 for origin port loc code")
    private String originPortLocCode;
    @Size(max=64, message = "max size is 64 for destination port loc code")
    private String destinationPortLocCode;
    private Boolean isSameAsOriginPort;
    private Boolean isSameAsDestinationPort;
    private String originCountry;
    private String destinationCountry;
    private String originPortCountry;
    private String destinationPortCountry;
    private String vehicleNumber;
    private String scacCode;
}