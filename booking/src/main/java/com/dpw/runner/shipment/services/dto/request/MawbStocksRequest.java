package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.List;

@Data
@Builder
@ApiModel("MAWB Stocks Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class MawbStocksRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
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
    private String borrowedFrom;
    private String borrowedFromFullName;
    private List<MawbStocksLinkRequest> mawbStocksLinkRows;
}
