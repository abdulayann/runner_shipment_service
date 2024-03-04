package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Map;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferServiceDetails implements IEntityTranferBaseEntity {
    public String serviceType;
    public EntityTransferParties contractor;
    public int srvLocation;
    public LocalDateTime bookingDate;
    public Long serviceCount;
    public Duration serviceDuration;
    public LocalDateTime completionDate;
    public String refNumber;
    public String serviceNotes;
    private Map<String, EntityTransferMasterLists> masterData;
}
