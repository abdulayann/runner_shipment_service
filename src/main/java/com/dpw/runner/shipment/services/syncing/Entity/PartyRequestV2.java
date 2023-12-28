package com.dpw.runner.shipment.services.syncing.Entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.util.Map;

@Data
public class PartyRequestV2 {
    @JsonProperty("AddressCode")
    private String AddressCode;
    @JsonProperty("AddressData")
    private Map<String, Object> AddressData;
    @JsonProperty("EntityID")
    private Long EntityID;
    @JsonProperty("EntityType")
    private String EntityType;
    @JsonProperty("OrgCode")
    private String OrgCode;
    @JsonProperty("OrgData")
    private Map<String, Object> OrgData;
    @JsonProperty("TenantId")
    private int TenantId;
    @JsonProperty("Type")
    private String Type;
    @JsonProperty("IsFreeTextAddress")
    private Boolean IsFreeTextAddress;
}
