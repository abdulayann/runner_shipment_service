package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
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
    private ListContractRequest listContractRequest;
}
