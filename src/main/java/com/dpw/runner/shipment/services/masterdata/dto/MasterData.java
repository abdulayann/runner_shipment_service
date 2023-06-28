package com.dpw.runner.shipment.services.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MasterData {
    private int id;
    private int itemType;
    private String itemValue;
    private String itemDescription;
    private String valuenDesc;
    private String cascade;
    private String identifier1;
    private String identifier2;
    private String identifier3;
    private String identifier4;
    private String identifier5;
    private String airLinePrefixValue;
    private String carriersLogo;
    private boolean isUnionTerritory;
    private String typeOfPayLoad;
    private String dpaManifestMaster;
}
