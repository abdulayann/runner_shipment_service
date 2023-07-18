package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;


@Entity
@Setter
@Getter
@Table(name = "allocations")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class Allocations extends MultiTenancy {

    @Column(name = "shipments_count")
    private Integer shipmentsCount;

    @Column(name = "hazardous")
    private Boolean hazardous;

    @Column(name = "cutoff_date")
    private LocalDateTime cutoffDate;

    @Column(name = "is_temperature_controlled")
    private Boolean isTemperatureControlled;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "weight_unit")
    private String weightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    private String volumeUnit;

    @Column(name = "chargeable")
    private BigDecimal chargable;

    @Column(name = "chargeable_unit")
    private String chargeableUnit;

    @Column(name = "min_temp")
    private BigDecimal minTemp;

    @Column(name = "min_temp_unit")
    private String minTempUnit;

    @Column(name = "max_temp")
    private BigDecimal maxTemp;

    @Column(name = "max_temp_unit")
    private String maxTempUnit;


}
