package com.dpw.runner.shipment.services.commons.requests;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class RunnerEntityMapping {
    private String parentTable;
    private String fieldName;
    private String tableName;
    private Class dataType;
    private boolean isContainsText;
}
