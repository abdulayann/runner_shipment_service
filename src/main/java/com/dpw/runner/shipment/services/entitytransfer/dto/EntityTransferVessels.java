package com.dpw.runner.shipment.services.entitytransfer.dto;


import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferVessels implements IEntityTranferBaseEntity, Serializable {
    public UUID Guid;
    public String Imo;
    public String Mmsi;
    public String Name;
    public String Flag;
    public String Type;
}
