package com.dpw.runner.shipment.services.migration.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import java.time.Instant;
import java.util.UUID;

@Entity
@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "consolidation_backup")
public class ConsolidationBackupEntity extends MultiTenancy {

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "consolidation_guid")
    private UUID consolidationGuid;

    @Column(name = "consolidation_details", columnDefinition = "jsonb")
    private String consolidationDetails;

    @Column(name = "back_up_time_utc", nullable = false)
    private Instant backUpTimeUtc = Instant.now();

    @Column(name = "console_shipment_mapping_details", columnDefinition = "jsonb")
    private String consoleShipmentMapping;

    @Column(name = "network_transfer_details", columnDefinition = "jsonb")
    private String networkTransferDetails;
}
