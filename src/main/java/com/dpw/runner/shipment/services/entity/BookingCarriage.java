package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "booking_carriage")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class BookingCarriage extends BaseEntity {

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
}
