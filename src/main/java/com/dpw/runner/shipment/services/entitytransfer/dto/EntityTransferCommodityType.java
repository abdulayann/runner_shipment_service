package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferCommodityType implements IEntityTranferBaseEntity, Serializable {
    public String Code;
    public String Description;
    public String HSCode;
    public Double GstPercentage;
    public String TaxUniqueCode;
    public Boolean Hazardous;
    public Long DGSubstanceId;
    public Long UNIDNo;
    public String DGClass;
    public String FlashPoint;
    public Boolean IsGlobal;
    public String CommodityDescriptionWithHSCode;
}
