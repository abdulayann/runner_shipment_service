package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferShipmentDetails;
import lombok.*;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ImportShipmentRequest implements IRunnerRequest {
    EntityTransferShipmentDetails entityData;
    private Long taskId;
    private String operation;
    private String rejectRemarks;
    private Boolean isFromNte;
    private String assignedTo;
}
