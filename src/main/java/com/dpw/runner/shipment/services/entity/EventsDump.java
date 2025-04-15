package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;


@Entity
@Setter
@Getter
@Table(name = "events_dump")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE events SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class EventsDump extends MultiTenancy {

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    @Size(max=100, message = "max size is 100 for entity_type")
    private String entityType;

    @Column(name = "event_code")
    @Size(max=210, message = "max size is 210 for event_code")
    private String eventCode;

    @Column(name = "description")
    @Size(max=512, message = "max size is 512 for description")
    private String description;

    @Column(name = "estimated")
    private LocalDateTime estimated;

    @Column(name = "actual")
    private LocalDateTime actual;

    @Column(name = "is_public_tracking_event")
    private Boolean isPublicTrackingEvent;

    @Column(name = "place_name")
    @Size(max=100, message = "max size is 100 for place_name")
    private String placeName;

    @Column(name = "place_description")
    @Size(max=100, message = "max size is 100 for place_description")
    private String placeDescription;

    @Column(name = "latitude")
    @Size(max=100, message = "max size is 100 for latitude")
    private String latitude;

    @Column(name = "longitude")
    @Size(max=100, message = "max size is 100 for longitude")
    private String longitude;

    @Column(name = "source")
    private String source;

    @Column(name = "event_estimate_update_reasons")
    private String event_estimate_update_reasons;

    @Column(name = "status")
    private String status;

    @Column(name = "pieces")
    private Integer pieces;

    @Column(name = "total_pieces")
    private Integer totalPieces;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "total_weight")
    private BigDecimal totalWeight;

    @Column(name = "is_partial")
    private Boolean isPartial;

    @Column(name = "received_date")
    private LocalDateTime receivedDate;

    @Column(name = "scheduled_date")
    private LocalDateTime scheduledDate;

}
