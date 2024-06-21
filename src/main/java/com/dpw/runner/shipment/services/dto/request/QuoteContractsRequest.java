package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class QuoteContractsRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private String contractId;
    private List<String> containerTypes;
}
