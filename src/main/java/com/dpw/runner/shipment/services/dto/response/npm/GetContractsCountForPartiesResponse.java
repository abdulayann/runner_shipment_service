package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

import java.util.List;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class GetContractsCountForPartiesResponse implements IRunnerResponse {
    private List<PartyContractsCountResponse> partyContractsCount;
    private int totalContractCount;
}
