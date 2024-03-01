package com.dpw.runner.shipment.services.commons.requests;

import lombok.*;

import java.io.Serializable;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuditLogChanges implements Serializable {
    private String fieldName;
    private Object oldValue;
    private Object newValue;
}
