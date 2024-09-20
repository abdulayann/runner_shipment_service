package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import lombok.*;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
public class SendConsolidationResponse implements IRunnerResponse {
    private List<Integer> successTenantIds;
    private String jsonString;
    private String message;
}
