package com.dpw.runner.shipment.services.dto.GeneralAPIRequests;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

@Data
public class AuditLogCreateRequest implements IRunnerRequest {
    private String type;
    private Long entityId;
}
