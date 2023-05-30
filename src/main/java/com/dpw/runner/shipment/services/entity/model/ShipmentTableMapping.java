package com.dpw.runner.shipment.services.entity.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class ShipmentTableMapping {
    private String parentTable;
    private String fieldName;
    private String tableName;
    private Class dataType;
}
