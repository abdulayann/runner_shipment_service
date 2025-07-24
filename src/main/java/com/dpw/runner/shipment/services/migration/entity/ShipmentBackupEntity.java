package com.dpw.runner.shipment.services.migration.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.time.Instant;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "shipment_backup")
@NoArgsConstructor
@AllArgsConstructor
public class ShipmentBackupEntity extends MultiTenancy {

    @Column(name = "back_up_time_utc", nullable = false)
    private Instant backupTimeUtc = Instant.now();

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "shipment_guid")
    private UUID shipmentGuid;

    @Column(name = "is_shipment_attached")
    private Boolean isShipmentAttached = Boolean.FALSE;

    @Column(name = "shipment_details", columnDefinition = "jsonb")
    private String shipmentDetail;

    @Column(name = "pickup_delivery_details", columnDefinition = "jsonb")
    private String pickupDeliveryDetail;

    @Column(name = "network_transfer_details", columnDefinition = "jsonb")
    private String networkTransferDetails;
}
