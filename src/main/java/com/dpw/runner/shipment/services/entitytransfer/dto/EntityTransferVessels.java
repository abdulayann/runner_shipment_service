package com.dpw.runner.shipment.services.entitytransfer.dto;


import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferVessels implements IEntityTranferBaseEntity, Serializable {
    public String Imo;
    public String Mmsi;
    public String Name;
    public String Flag;
    public String Type;
}
