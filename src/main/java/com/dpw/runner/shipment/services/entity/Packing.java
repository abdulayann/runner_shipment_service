package com.dpw.runner.shipment.services.entity;

import javax.persistence.*;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.dto.request.ContainerRequest;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import java.awt.*;
import java.math.BigDecimal;

@Entity
@Table(name = "packing")
@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE packing SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Packing extends MultiTenancy {

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "dg_goods_id")
    private Integer DGGoodsId;

    @Column(name = "dg_substance_id")
    private Integer DGSubstanceId;

    @Column(name = "packs")
    private String packs;

    @Column(name = "packs_type")
    private String packsType;

    @Column(name = "container_number")
    private String containerNumber;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "weight_unit")
    private String weightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    private String volumeUnit;

    @Column(name = "inspections")
    private String inspections;

    @Column(name = "origin")
    private String origin;

    @Column(name = "commodity")
    private String commodity;

    @Column(name = "packing_order")
    private String packingOrder;

    @Column(name = "length")
    private BigDecimal length;

    @Column(name = "length_unit")
    private String lengthUnit;

    @Column(name = "width")
    private BigDecimal width;

    @Column(name = "width_unit")
    private String widthUnit;

    @Column(name = "height")
    private BigDecimal height;

    @Column(name = "height_unit")
    private String heightUnit;

    @Column(name = "marksn_nums")
    private String marksnNums;

    @Column(name = "flash_point")
    private String flashPoint;

    @Column(name = "undg_contact")
    private String UNDGContact;

    @Column(name = "is_temperature_controlled")
    private Boolean isTemperatureControlled;

    @Column(name = "min_temp")
    private BigDecimal minTemp;

    @Column(name = "min_temp_unit_id")
    private String minTempUnit;

    @Column(name = "max_temp")
    private BigDecimal maxTemp;

    @JoinColumn(name = "max_temp_unit")
    private String maxTempUnit;

    @Column(name = "hs_code")
    private String HSCode;

    @Column(name = "country_code")
    private String countryCode;

    @Column(name = "goods_description")
    private String goodsDescription;

    @Column(name = "reference_number")
    private String referenceNumber;

    @Column(name = "dg_class_id")
    private String DGClass;

    @Column(name = "hazardous")
    private Boolean hazardous;

    @Column(name = "commodity_id")
    private Long commodityId;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    private String netWeightUnit;

    @Column(name = "volume_weight")
    private BigDecimal volumeWeight;

    @Column(name = "volume_weight_unit")
    private String volumeWeightUnit;

    @Column(name = "vin_number")
    private String vinNumber;

    @Column(name = "container_id")
    private Long containerId;

    @Column(name = "transport_mode")
    private String transportMode;

    @Column(name = "inner_package_number")
    private String innerPackageNumber;

    @Column(name = "inner_package_type_id")
    private String innerPackageType;

    @Column(name = "chargeable")
    private BigDecimal chargeable;

    @Column(name = "chargeable_unit")
    private String chargeableUnit;

    @Column(name = "customs_release_code")
    private String customsReleaseCode;

    @Column(name = "shipment_number")
    private String shipmentNumber;

    @Column(name = "inner_packs_id")
    private Long innerPacksId;

    @Column(name = "inner_packs_count")
    private Long innerPacksCount;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;

//    @ManyToOne
//    @JoinColumn(name = "container_id", insertable = false, updatable = false)
//    private Containers container;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "container_id", nullable = false, insertable = false, updatable = false)
    private Containers container;

}

