package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferV3ShipmentDetails;
import lombok.*;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ImportV3ShipmentRequest implements IRunnerRequest {
    EntityTransferV3ShipmentDetails entityData;
    private Long taskId;
    private String operation;
    private String rejectRemarks;
    private Boolean isFromNte;
    private String assignedTo;
}
