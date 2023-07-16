package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;


@Entity
@Data
@Table(name = "booking_carriage")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE booking_carriage SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class BookingCarriage extends MultiTenancy {

    @Column(name = "pol_id")
    private Long polId;

    @Column(name = "pod_id")
    private Long podId;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "vessel")
    private String vessel;

    @Column(name = "voyage")
    private String voyage;

    @Column(name = "carriage_type")
    private String carriageType;

    @Column(name = "carriage_mode")
    private String carriageMode;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "vessel_id")
    private Long vesselId;

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;
}
