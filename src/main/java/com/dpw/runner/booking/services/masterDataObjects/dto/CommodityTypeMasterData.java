package com.dpw.runner.booking.services.masterDataObjects.dto;

import com.dpw.runner.booking.services.masterDataObjects.common.request.IMasterDataBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CommodityTypeMasterData implements IMasterDataBaseEntity, Serializable {
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
