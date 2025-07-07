package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Carrier Details Response Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class CarrierDetailResponse implements IRunnerResponse {
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
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime eta;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime etd;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime ata;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    @ExcludeTimeZone
    private LocalDateTime atd;
    private String originPort;
    private String destinationPort;
    private String originPortName;
    private String destinationPortName;
    private String cfs;
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private Map<String, String> carrierMasterData;
    private Map<String, String> vesselsMasterData;
    private LocalDateTime vesselBerthingDate;
    private String voyageOrFlightNumber;
    private String carrierCountry;
    private String minTransitHours;
    private String maxTransitHours;
    private Boolean carrierAddedFromNpm;
    private Boolean isCarrierChanged;
    private String originLocCode;
    private String destinationLocCode;
    private String originPortLocCode;
    private String destinationPortLocCode;
    private Boolean isSameAsOriginPort;
    private Boolean isSameAsDestinationPort;
    private String originCountry;
    private String destinationCountry;
    private String originPortCountry;
    private String destinationPortCountry;
    private String vehicleNumber;
}
