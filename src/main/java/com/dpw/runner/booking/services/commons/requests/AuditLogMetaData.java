package com.dpw.runner.booking.services.commons.requests;

import com.dpw.runner.booking.services.entity.commons.BaseEntity;
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
}
