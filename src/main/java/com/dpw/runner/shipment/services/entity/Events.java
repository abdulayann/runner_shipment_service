package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "events")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class Events extends MultiTenancy {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    @ToString.Include
    private Long id;

    @ManyToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    @Where(clause = "entity_type = 'events'")
    private ShipmentDetails shipmentDetailsList;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "guid")
    private UUID guid;

    @Column(name = "master_list")
    private String masterList;

    @Column(name = "description")
    private String description;

    @Column(name = "estimated")
    private LocalDateTime estimated;

    @Column(name = "actual")
    private LocalDateTime actual;

    @Column(name = "is_public_tracking_event")
    private Boolean isPublicTrackingEvent;

    @Column(name = "place_name")
    private String placeName;

    @Column(name = "place_description")
    private String placeDescription;

    @Column(name = "latitude")
    private String latitude;

    @Column(name = "longitude")
    private String longitude;

    @Column(name = "source")
    private String source;

    @Column(name = "event_estimate_update_reasons")
    private String event_estimate_update_reasons;
}
