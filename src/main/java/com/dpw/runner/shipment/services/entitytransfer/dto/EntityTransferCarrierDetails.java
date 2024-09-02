package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
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
    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime ata;
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

    // Previous fields
//    private Map<String, EntityTransferMasterLists> masterData;
//    private Map<String, EntityTransferUnLocations> unlocationData;
//    private Map<String, EntityTransferCarrier> carrierMasterData;
//    private Map<String, EntityTransferVessels> vesselsMasterData;
}
