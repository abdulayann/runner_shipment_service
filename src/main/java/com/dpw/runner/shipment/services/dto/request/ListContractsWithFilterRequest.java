package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ListContractsWithFilterRequest implements IRunnerRequest {
    private String cargoType;
    private String origin;
    private String destination;
    private Boolean isDgEnabled = false;
    private String parentContractId;
    private Long minTransitDays;
    private Long maxTransitDays;
    private ListContractRequest listContractRequest;
    @Builder.Default
    private Integer pageNo = 1;
    @Builder.Default
    private Integer pageSize = Integer.MAX_VALUE;
    private SortRequest sortRequest;
}
