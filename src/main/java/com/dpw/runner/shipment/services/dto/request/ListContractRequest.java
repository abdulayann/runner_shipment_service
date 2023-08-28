package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ListContractRequest implements IRunnerRequest {
    private String customer_org_id;
    private String org_role;
    private List<String> filter_contract_states;
}
