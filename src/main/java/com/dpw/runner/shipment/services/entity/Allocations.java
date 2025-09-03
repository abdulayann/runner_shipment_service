package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;


@Entity
@Setter
@Getter
@Table(name = "allocations")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@BatchSize(size = 50)
public class Allocations extends MultiTenancy {

    @Column(name = "shipments_count")
    private Integer shipmentsCount;

    @Column(name = "cutoff_date")
    private LocalDateTime cutoffDate;

    @Column(name = "is_temperature_controlled")
    private Boolean isTemperatureControlled;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String weightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    @Column(name = "chargeable")
    private BigDecimal chargable;

    @Column(name = "chargeable_unit")
    private String chargeableUnit;

    @Column(name = "min_temp")
    private BigDecimal minTemp;

    @Column(name = "min_temp_unit")
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    private String minTempUnit;

    @Column(name = "max_temp")
    private BigDecimal maxTemp;

    @Column(name = "max_temp_unit")
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    private String maxTempUnit;

    @Column(name = "weight_volume")
    private BigDecimal weightVolume;

    @Column(name = "weight_volume_unit")
    private String weightVolumeUnit;

    @Column(name = "packs")
    private Integer packs;

    @Column(name = "packs_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsType;

    @Column(name = "dg_packs")
    private Integer dgPacks;

    @Column(name = "dg_packs_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String dgPacksType;

    @Column(name = "dg_container_count")
    private Integer dgContainerCount;

    @Column(name = "container_count")
    private Integer containerCount;

    @Column(name = "teu_count")
    private BigDecimal teuCount;

    @Column(name = "slac_count")
    private Integer slacCount;

}
