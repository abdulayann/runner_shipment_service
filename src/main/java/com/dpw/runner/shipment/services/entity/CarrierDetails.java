package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.GenerationTime;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "carrier_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE carrier_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class CarrierDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Column(name = "shipping_line")
    private String shippingLine;

    @Column(name = "vessel")
    private String vessel;

    @Column(name = "voyage")
    private String voyage;

    @Column(name = "flight_number")
    private String flightNumber;

    @Column(name = "aircraft_type")
    private String aircraftType;

    @Column(name = "aircraft_registration")
    private String aircraftRegistration;

    @Column(name = "truck_ref_number")
    private String truckRefNumber;

    @Column(name = "journey_number")
    private String journeyNumber;

    @Column(name = "journey_ref_number")
    private String journeyRefNumber;

    @Column(name = "origin")
    private String origin;

    @Column(name = "destination")
    private String destination;

    @Column(name = "eta")
    private LocalDateTime eta;

    @Column(name = "etd")
    private LocalDateTime etd;

    @Column(name = "ata")
    private LocalDateTime ata;

    @Column(name = "atd")
    private LocalDateTime atd;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;
}
