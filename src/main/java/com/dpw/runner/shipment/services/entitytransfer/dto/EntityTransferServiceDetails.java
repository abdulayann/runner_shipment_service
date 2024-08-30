package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Map;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferServiceDetails implements IEntityTranferBaseEntity {
    private String serviceType;
    private EntityTransferParties contractor;
    private String srvLocation;
    private LocalDateTime bookingDate;
    private Long serviceCount;
    private LocalTime serviceDuration;
    private LocalDateTime completionDate;
    private String refNumber;
    private String serviceNotes;
    private Map<String, EntityTransferMasterLists> masterData;
}
