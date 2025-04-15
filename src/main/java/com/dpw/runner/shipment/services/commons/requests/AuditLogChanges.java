package com.dpw.runner.shipment.services.commons.requests;

import java.io.Serializable;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
