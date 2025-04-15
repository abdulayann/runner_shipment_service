package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferCarrierDetails implements IEntityTranferBaseEntity {
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

    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private Map<String, String> carrierMasterData;
    private Map<String, String> vesselsMasterData;

}
