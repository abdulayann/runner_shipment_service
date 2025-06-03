package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;

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
@BatchSize(size = 50)
public class AchievedQuantities extends MultiTenancy {

    @Column(name = "weight_volume")
    private BigDecimal weightVolume;

    @Column(name = "weight_volume_unit")
    private String weightVolumeUnit;

    @Column(name = "consolidated_weight")
    private BigDecimal consolidatedWeight;

    @Column(name = "consolidated_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String consolidatedWeightUnit;

    @Column(name = "consolidated_volume")
    private BigDecimal consolidatedVolume;

    @Column(name = "consolidated_volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String consolidatedVolumeUnit;

    @Column(name = "consolidation_charge_quantity")
    private BigDecimal consolidationChargeQuantity;

    @Column(name = "consolidation_charge_quantity_unit")
    private String consolidationChargeQuantityUnit;

    @Column(name = "weight_utilization")
    private String weightUtilization;

    @Column(name = "volume_utilization")
    private String volumeUtilization;

    @Column(name = "packs")
    private Integer packs;

    @Column(name = "packs_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsType;

    @Column(name = "packs")
    private Integer dgPacks;

    @Column(name = "packs_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String dgPacksType;

    @Column(name = "dg_container_count")
    private Integer dgContainerCount;

    @Column(name = "slac_count")
    private Integer slacCount;

    @Column(name = "container_count")
    private Integer containerCount;

    @Column(name = "teu_count")
    private BigDecimal teuCount;

}
