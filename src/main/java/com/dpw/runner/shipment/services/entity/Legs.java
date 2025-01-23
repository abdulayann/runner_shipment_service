package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;

import com.dpw.runner.shipment.services.utils.OrganizationData;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "legs")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class Legs extends MultiTenancy {

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
    @JoinColumn(name = "origin_address_id", referencedColumnName = "id")
    @OrganizationData
    private Parties originAddress;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "destination_id", referencedColumnName = "id")
    @OrganizationData
    private Parties destination;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "destination_address_id", referencedColumnName = "id")
    @OrganizationData
    private Parties destinationAddress;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = TiReferences.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "reference_id", referencedColumnName = "id")
    private TiReferences tiReferences;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = TiTruckDriverDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "ti_truck_driver_id", referencedColumnName = "id")
    private TiTruckDriverDetails tiTruckDriverDetails;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = TiContainers.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "ti_container_id", referencedColumnName = "id")
    private TiContainers tiContainers;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = TiPackages.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "ti_package_id", referencedColumnName = "id")
    private TiPackages tiPackages;

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
