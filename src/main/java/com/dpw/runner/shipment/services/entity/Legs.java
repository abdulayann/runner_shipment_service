package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
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

    @Column(name = "origin")
    private String origin;

    @Column(name = "origin_address")
    private String originAddress;

    @Column(name = "destination")
    private String destination;

    @Column(name = "destination_address")
    private String destinationAddress;

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
