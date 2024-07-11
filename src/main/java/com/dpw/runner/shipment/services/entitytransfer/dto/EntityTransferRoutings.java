package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
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
    private Long polId;
    private Long podId;
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
    private Long routeLegId;
    private Long vesselId;
    private Long transitDays;
    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferUnLocations> unlocationData;
}
