package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;
import org.apache.poi.hpsf.GUID;

import java.util.Map;
import java.util.UUID;

@Data
public class PartyRequestV2 {
    private String AddressCode;
    private Map<String, Object> AddressData;
    private long EntityID;
    private String EntityType;
    private String OrgCode;
    private Map<String, Object> OrgData;
    private int TenantId;
    private String Type;
    private UUID Guid;
    private long Id;
}
