package com.dpw.runner.shipment.services.migration.entity;

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
public class ConsolidationBackupEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private Integer tenantId;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "consolidation_guid")
    private UUID consolidationGuid;

    @Column(name = "consolidation_details", columnDefinition = "jsonb")
    private String consolidationDetails;

    @Column(name = "back_up_time_utc", nullable = false)
    private Instant backUpTimeUtc = Instant.now();
}
