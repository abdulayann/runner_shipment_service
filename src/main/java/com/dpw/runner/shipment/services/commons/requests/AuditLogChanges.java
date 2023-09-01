package com.dpw.runner.shipment.services.commons.requests;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuditLogChanges {
    private String fieldName;
    private Object oldValue;
    private Object newValue;
}
