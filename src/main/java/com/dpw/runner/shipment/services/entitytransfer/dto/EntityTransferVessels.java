package com.dpw.runner.shipment.services.entitytransfer.dto;


import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferVessels implements IEntityTranferBaseEntity {
    public String Imo;
    public String Mmsi;
    public String Name;
    public String Flag;
    public String Type;
}
