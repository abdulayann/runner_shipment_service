package com.dpw.runner.shipment.services.entitytransfer.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class UpdateStatusFromExternalRequest implements IRunnerRequest {
    private String entityGuid;
    private String entityType;
    private String createdBy;
    private String operation;
    private String rejectRemarks;
    private int destinationTenantId;
    private String destinationEntityGuid;
}
