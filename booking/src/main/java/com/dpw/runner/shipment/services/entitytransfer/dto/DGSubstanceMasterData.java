package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IMasterDataBaseEntity;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DGSubstanceMasterData implements IMasterDataBaseEntity, Serializable {
    public Long Id;
    public Long DGGoodsId;
    public Long UNIDNo;

    public String Variant;
    public String Variation;
    public String ProperShippingName;
    public String ClassDivision;
    public String PG;
    public String MP;
    public String FlashPoint;
    public String SubLabelRisk1;
    public String SubLabelRisk2;
    public String EMS;
    public String State;
    public String Active;
    public String System;
    public String USDOTShippingName;
    public String StowageRequirements;
    public String StowageCategory;
    public String TreatAs;
    public String TechnicalName;
    public String SPAir;
    public String ExceptedQuantity;
    public String LQPackMaxQty;
    public String LQPackUQ;
    public String LQPackingInstruction;
    public String CAOPackMaxQty;
    public String CAOPackUQ;
    public String CAOPackingInstruction;
    public String PAXCAPackMaxQty;
    public String PAXCAPackUQ;
    public String PAXCAPackingInstruction;
    public String ClassName;
    public String DGClass;
}
