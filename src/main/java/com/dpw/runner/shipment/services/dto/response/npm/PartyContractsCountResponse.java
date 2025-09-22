package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PartyContractsCountResponse implements IRunnerResponse {
    private String customerOrgId;
    private int contractCount;
}
