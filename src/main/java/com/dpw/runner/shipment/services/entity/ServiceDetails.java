package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.experimental.Accessors;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.Duration;
import java.time.LocalDateTime;


@Entity
@Setter
@Getter
@Table(name = "services")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE services SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ServiceDetails extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "service_type")
    private String serviceType;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "contractor_id", referencedColumnName = "id")
    private Parties contractor;

    @Column(name = "srv_location")
    private int srvLocation;

    @Column(name = "booking_date")
    private LocalDateTime bookingDate;

    @Column(name = "service_count")
    private Long serviceCount;

    @Column(name = "service_duration")
    private Duration serviceDuration;

    @Column(name = "completion_date")
    private LocalDateTime completionDate;

    @Column(name = "ref_number")
    private String refNumber;

    @Column(name = "service_notes")
    private String serviceNotes;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;
}
