package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferConsolidationDetails;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ConsolidationDetails;
import lombok.*;

import java.util.Map;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ImportV3ConsolidationRequest implements IRunnerRequest {
    private EntityTransferV3ConsolidationDetails entityData;
    private Long taskId;
    private String operation;
    private String rejectRemarks;
    private Boolean isFromNte;
    private Map<String, String> shipmentNumberAssignedToMap;

}
