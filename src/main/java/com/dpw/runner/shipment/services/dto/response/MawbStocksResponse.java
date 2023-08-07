package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.MawbStocksLink;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class MawbStocksResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long consolidationId;
    private String mawbNumber;
    private String nextMawbNumber;
    private String availableCount;
    private String status;
    private Long homePort;
    private String airLinePrefix;
    private String prefix;
    private String seqNumber;
    private String count;
    private Long startNumber;
    private String from;
    private String to;
    private long borrowedFrom;
    private String borrowedFromFullName;
    private List<MawbStocksLinkResponse> mawbStocksLinkRows;
}
