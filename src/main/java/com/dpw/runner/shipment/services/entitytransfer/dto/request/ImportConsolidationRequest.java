package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import lombok.*;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ImportConsolidationRequest implements IRunnerRequest {
    EntityTransferConsolidationDetails entityTransferConsolidationDetails;
    Long taskId;

}
