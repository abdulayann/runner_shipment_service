package com.dpw.runner.booking.services.masterdata.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MasterData implements Serializable {
    @JsonProperty("Id")
    private int id;
    @JsonProperty("ItemType")
    private int itemType;
    @JsonProperty("ItemValue")
    private String itemValue;
    @JsonProperty("ItemDescription")
    private String itemDescription;
    @JsonProperty("ValuenDesc")
    private String valuenDesc;
    @JsonProperty("Cascade")
    private String cascade;
    @JsonProperty("Identifier1")
    private String identifier1;
    @JsonProperty("Identifier2")
    private String identifier2;
    @JsonProperty("Identifier3")
    private String identifier3;
    @JsonProperty("Identifier4")
    private String identifier4;
    @JsonProperty("Identifier5")
    private String identifier5;
    @JsonProperty("AirLinePrefixValue")
    private String airLinePrefixValue;
    @JsonProperty("CarriersLogo")
    private String carriersLogo;
    @JsonProperty("IsUnionTerritory")
    private boolean isUnionTerritory;
    @JsonProperty("TypeOfPayLoad")
    private String typeOfPayLoad;
    @JsonProperty("DpaManifestMaster")
    private String dpaManifestMaster;
}
