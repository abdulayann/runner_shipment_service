package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.time.LocalDateTime;
import java.util.Map;
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class EntityTransferArrivalDepartureDetails implements IEntityTranferBaseEntity {
    private EntityTransferParties containerYardId;
    private EntityTransferParties transportPortId;
    private EntityTransferParties CTOId;
    private EntityTransferParties CFSId;
    private EntityTransferParties firstForeignPortId;
    private EntityTransferParties lastForeignPortId;
    private String firstForeignPort;
    private String lastForeignPort;
    private String type;
    private LocalDateTime firstForeignPortArrivalDate;
    private LocalDateTime lastForeignPortDepartureDate;
    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferUnLocations> unlocationData;
}
