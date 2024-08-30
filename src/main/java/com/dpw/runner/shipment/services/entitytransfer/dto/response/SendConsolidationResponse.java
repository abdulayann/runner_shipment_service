package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import lombok.*;

import java.util.List;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class SendConsolidationResponse implements IRunnerResponse {
    private List<Integer> successTenantIds;
    EntityTransferConsolidationDetails entityTransferConsolidationDetails;
    private String jsonString;
}
