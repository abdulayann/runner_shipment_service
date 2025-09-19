package com.dpw.runner.shipment.services.dto.response.npm;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.*;

@Getter
@Setter
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PartyContractsCountResponse {
    private String customerOrgId;
    private int contractCount;
}
