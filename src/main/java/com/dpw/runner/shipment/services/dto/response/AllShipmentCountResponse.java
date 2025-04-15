package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AllShipmentCountResponse implements IRunnerResponse {

    private Long attachedShipmentCurrentBranchCount = 0L;
    private Long attachedShipmentInterBranchCount = 0L;
    private Long pendingAttachmentCount = 0L;
}
