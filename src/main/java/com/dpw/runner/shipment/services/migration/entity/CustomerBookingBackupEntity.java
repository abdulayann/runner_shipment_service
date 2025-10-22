package com.dpw.runner.shipment.services.migration.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "customer_booking_backup")
@NoArgsConstructor
@AllArgsConstructor
public class CustomerBookingBackupEntity extends MultiTenancy {


    @Column(name = "back_up_time_utc", nullable = false)
    private Instant backupTimeUtc = Instant.now();

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "booking_guid")
    private UUID bookingGuid;

    @Column(name = "customer_booking_details", columnDefinition = "jsonb")
    private String customerBookingDetails;
}
