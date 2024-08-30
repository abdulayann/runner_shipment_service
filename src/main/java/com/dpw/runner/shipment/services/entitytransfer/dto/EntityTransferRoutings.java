package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;

@ToString
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EntityTransferRoutings implements IEntityTranferBaseEntity {
    private Long leg;
    private String mode;
    private String routingStatus;
    private String vesselName;
    private String pol;
    private String pod;
    @JsonProperty("domestic")
    private boolean isDomestic;
    private LocalDateTime eta;
    private LocalDateTime etd;
    private LocalDateTime ata;
    private LocalDateTime atd;
    private Boolean isLinked;
    private String voyage;
    private String aircraftRegistration;
    private String flightNumber;
    private String aircraftType;
    private String entityType;
    private Long entityId;
    private Long routeLegId;
    private Long transitDays;
    private String carrier;
    private String truckReferenceNumber;
    private String carrierCountry;
    private Map<String, String> unlocationData;
    private Map<String, String> masterData;
    private Map<String, String> carrierMasterData;

    // Previous
//    private Map<String, EntityTransferMasterLists> masterData;
//    private Map<String, EntityTransferUnLocations> unlocationData;
}
