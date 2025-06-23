package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.EventType;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

@Entity
@Setter
@Getter
@Table(name = "events")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE events SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Events extends MultiTenancy {

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    @Size(max=100, message = "max size is 100 for entity_type")
    private String entityType;

    @Column(name = "event_code")
    @Size(max=210, message = "max size is 210 for event_code")
    @MasterData(type = MasterDataType.ORDER_EVENTS)
    private String eventCode;

    @Column(name = "event_type")
    @Enumerated(EnumType.STRING)
    private EventType eventType;

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
    @MasterData(type = MasterDataType.EVENT_SOURCE)
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

    @Column(name = "container_number")
    private String containerNumber;

    @Column(name = "location_role")
    @MasterData(type = MasterDataType.LOCATION_ROLE)
    private String locationRole;

    @Column(name = "flight_number")
    private String flightNumber;

    @Column(name = "flight_name")
    private String flightName;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "shipment_number")
    private String shipmentNumber;

    @Column(name = "direction")
    private String direction;

    @Column(name = "remarks")
    private String remarks;

    @Column(name = "user_name")
    private String userName;

    @Column(name = "user_email")
    private String userEmail;

    @Column(name = "branch")
    private String branch;

    @Column(name = "reference_number")
    private String referenceNumber;

}
