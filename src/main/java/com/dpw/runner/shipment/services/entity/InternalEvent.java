package com.dpw.runner.shipment.services.entity;

import lombok.*;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "internal_events")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InternalEvent {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "entity_id", nullable = false)
    private Long entityId;

    @Column(name = "entity_type", nullable = false)
    private String entityType;

    @Column(name = "payload", columnDefinition = "TEXT")
    private String payload;

    @Column(name = "published_status")
    private String publishedStatus;

    @Column(name = "published_timestamp")
    private LocalDateTime publishedTimestamp;

    @Column(name = "consumed_status")
    private String consumedStatus;

    @Column(name = "consumed_timestamp")
    private LocalDateTime consumedTimestamp;
}
