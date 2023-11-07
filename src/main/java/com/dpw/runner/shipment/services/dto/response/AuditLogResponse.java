package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class AuditLogResponse implements IRunnerResponse {
    private long id;
    private String operation;
    private String entity;
    private Long entityId;
    private List<AuditLogChanges> changes;
    private String parentType;
    private Long parentId;
    private LocalDateTime createdAt;
    private String createdBy;
}
