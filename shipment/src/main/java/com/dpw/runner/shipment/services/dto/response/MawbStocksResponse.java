package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
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
    private List<MawbStocksLinkResponse> mawbStocksLinkRows;
}
