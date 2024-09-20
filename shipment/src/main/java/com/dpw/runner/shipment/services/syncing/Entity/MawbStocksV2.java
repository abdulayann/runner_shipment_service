package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class MawbStocksV2 implements IRunnerRequest {
    private Long consolidationId;
    private String mawbNumber;
    private String nextMawbNumber;
    private String availableCount;
    private String status;
    private String homePort;
    private String airLinePrefix;
    private String prefix;
    private String seqNumber;
    private String count;
    private Long startNumber;
    private String from;
    private String to;
    private String borrowedFrom;
    private String borrowedFromFullName;
    private List<MawbStocksLinkV2> mawbStocksLinkRows;

    private UUID guid;
    private UUID consolidationGuid;
}
