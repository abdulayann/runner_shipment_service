package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferMasterLists implements IEntityTranferBaseEntity {
    public int ItemType;
    public String ItemValue;
    public String ItemDescription;
    public String ValuenDesc;
    public String Cascade;
    public String Identifier1;
    public String Identifier2;
    public String Identifier3;
    public String Identifier4;
    public String Identifier5;
    public String AirLinePrefixValue;
    public String CarriersLogo;
    public Boolean IsUnionTerritory;
    public String TypeOfPayLoad;
    public String DPAManifestMaster;
}