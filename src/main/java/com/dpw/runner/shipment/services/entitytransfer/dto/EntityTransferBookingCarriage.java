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
public class EntityTransferBookingCarriage implements IEntityTranferBaseEntity {
    public Long polId;
    public Long podId;
    public LocalDateTime eta;
    public LocalDateTime etd;
    public String vessel;
    public String voyage;
    public String carriageType;
    public String carriageMode;
    public Long vesselId;
    public Map<String, EntityTransferMasterLists> masterData;
    public Map<String, EntityTransferUnLocations> unlocationData;
    public Map<String, EntityTransferVessels> vesselsMasterData;
}
