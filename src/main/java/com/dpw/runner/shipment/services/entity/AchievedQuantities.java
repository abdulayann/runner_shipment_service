package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;


@Entity
@Setter
@Getter
@Table(name = "achieved_quantities")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class AchievedQuantities extends MultiTenancy {

    @Column(name = "weight_volume")
    private BigDecimal weightVolume;

    @Column(name = "weight_volume_unit")
    private String weightVolumeUnit;

    @Column(name = "consolidated_weight")
    private BigDecimal consolidatedWeight;

    @Column(name = "consolidated_weight_unit")
    private String consolidatedWeightUnit;

    @Column(name = "consolidated_volume")
    private BigDecimal consolidatedVolume;

    @Column(name = "consolidated_volume_unit")
    private String consolidatedVolumeUnit;

    @Column(name = "consolidation_charge_quantity")
    private BigDecimal consolidationChargeQuantity;

    @Column(name = "consolidation_charge_quantity_unit")
    private String consolidationChargeQuantityUnit;

    @Column(name = "weight_utilization")
    private String weightUtilization;

    @Column(name = "volume_utilization")
    private String volumeUtilization;

}
