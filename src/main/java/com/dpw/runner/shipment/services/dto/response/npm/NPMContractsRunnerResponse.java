package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@SuppressWarnings("java:S1948")
public class NPMContractsRunnerResponse implements IRunnerResponse {
    private List<NPMContractsResponse.NPMContractResponse> contracts;
    private String parent_contract_id;
}

