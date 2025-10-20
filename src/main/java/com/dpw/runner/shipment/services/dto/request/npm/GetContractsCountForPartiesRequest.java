package com.dpw.runner.shipment.services.dto.request.npm;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GetContractsCountForPartiesRequest implements IRunnerRequest {
    private String cargoType;
    private String origin;
    private String destination;
    private Boolean isDgEnabled = false;
    private List<String> customerOrgIds;
    private ListContractRequest contractsCountRequest;
}
