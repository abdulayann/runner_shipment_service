package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "ti_legs")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE ti_legs SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class TiLegs extends MultiTenancy {

    @Column(name = "pickup_delivery_details_id")
    private Long pickupDeliveryDetailsId;

    @Column(name = "sequence")
    private Long sequence;

    @NotNull
    @Column(name = "leg_type")
    private String legType;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "origin_id", referencedColumnName = "id")
    @OrganizationData
    private Parties origin;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "destination_id", referencedColumnName = "id")
    @OrganizationData
    private Parties destination;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "ti_leg_id")
    @BatchSize(size = 50)
    private List<TiReferences> tiReferences;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "ti_leg_id")
    @BatchSize(size = 50)
    private List<TiTruckDriverDetails> tiTruckDriverDetails;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "ti_leg_id")
    @BatchSize(size = 50)
    private List<TiContainers> tiContainers;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "ti_leg_id")
    @BatchSize(size = 50)
    private List<TiPackages> tiPackages;

    @Column(name = "estimated_pickup")
    private LocalDateTime estimatedPickup;

    @Column(name = "estimated_delivery")
    private LocalDateTime estimatedDelivery;

    @Column(name = "actual_pickup")
    private LocalDateTime actualPickup;

    @Column(name = "actual_delivery")
    private LocalDateTime actualDelivery;

    @Column(name = "required_by")
    private LocalDateTime requiredBy;

    @Column(name = "drop_mode")
    private String dropMode;

    @Column(name = "remarks")
    private String remarks;
}
