package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.io.Serializable;
import java.util.UUID;

@Data
public class MawbStocksLinkV2 implements Serializable {
    private Long parentId;
    private String mawbNumber;
    private String status;
    private String seqNumber;
    private String entityType;
    private Long entityId;
    private String shipConsNumber;

    private UUID guid;
    private Integer tenantId;
    private UUID shipmentGuid;
    private UUID consolidationGuid;
}
