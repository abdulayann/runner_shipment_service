package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
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
    private RoutingCarriage carriage;
    private String routingStatus;
    private String vesselName;
    private String pol;
    private String pod;
    @JsonProperty("domestic")
    private boolean isDomestic;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime eta;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime etd;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime ata;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
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

}
