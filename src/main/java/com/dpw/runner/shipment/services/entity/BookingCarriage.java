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

    @Column(name = "port_of_loading")
    private String portOfLoading;

    @Column(name = "port_of_discharge")
    private String portOfDischarge;

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

}
