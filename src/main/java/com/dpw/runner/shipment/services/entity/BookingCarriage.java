package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "booking_carriage")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class BookingCarriage extends MultiTenancy {

    @Id
    @ToString.Include
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

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

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'BookingCarriage'")
    private List<ShipmentDetails> shipmentDetailsList;

    @Column(name = "vessel_id")
    private Long vesselId;

    @Column(name = "booking_id")
    private Long bookingId;
}
