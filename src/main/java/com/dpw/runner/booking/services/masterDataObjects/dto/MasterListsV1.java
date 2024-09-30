package com.dpw.runner.booking.services.masterDataObjects.dto;

import com.dpw.runner.booking.services.masterDataObjects.common.request.IMasterDataBaseEntity;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.io.Serializable;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MasterListsV1 implements IMasterDataBaseEntity, Serializable {
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
    @JsonProperty("TenantId")
    public Integer tenantId;
}