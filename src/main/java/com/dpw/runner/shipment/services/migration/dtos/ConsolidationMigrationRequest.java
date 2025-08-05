package com.dpw.runner.shipment.services.migration.dtos;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class ConsolidationMigrationRequest {
    private Integer tenantId;

    private Long consolId;

    private Long bookingId;

}
