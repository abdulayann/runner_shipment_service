package com.dpw.runner.shipment.services.migration.dtos;

import lombok.Generated;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Generated
public class BookingMigrationRequest {
    private Integer tenantId;

    private Long bookingId;
}
