package com.dpw.runner.shipment.services.migration.dtos;

import lombok.Generated;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
@Generated
public class ConsolidationMigrationRequest {
    private Integer tenantId;

    private Long consolId;

    private Integer count;

    private Long bookingId;

}
