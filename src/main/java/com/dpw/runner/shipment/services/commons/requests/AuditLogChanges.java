package com.dpw.runner.shipment.services.commons.requests;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class AuditLogChanges {
    private String fieldName;
    private Object oldValue;
    private Object newValue;
}
