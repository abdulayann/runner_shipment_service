package com.dpw.runner.shipment.services.commons.requests;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;

@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class AuditLogMetaData {
    private BaseEntity newData;
    private BaseEntity prevData;
    private String operation;
    private String parent;
    private Long parentId;
    private String entityType;
    private String userName;
    private Integer tenantId;
    private Boolean isIntegrationLog;
    private String flow;
    private String dataType;
}
