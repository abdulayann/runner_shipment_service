package com.dpw.runner.shipment.services.migration.dtos;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class BookingMigrationRequest {
    private Integer tenantId;

    private Long bookingId;
}
