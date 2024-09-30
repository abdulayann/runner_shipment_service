package com.dpw.runner.booking.services.dto.response.npm;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class NPMContractsRunnerResponse implements IRunnerResponse {
    private List<NPMContractsResponse.NPMContractResponse> contracts;
    private String parent_contract_id;
}

