package com.dpw.runner.shipment.services.entity;

import javax.persistence.*;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

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

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "dg_goods_id")
    private Integer DGGoodsId;

    @Column(name = "dg_substance_id")
    private Integer DGSubstanceId;

    @Column(name = "packs")
    private String packs;

    @Column(name = "packs_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsType;

    @Column(name = "container_number")
    private String containerNumber;

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

    @Column(name = "inspections")
    private String inspections;

    @Column(name = "origin")
    private String origin;

    @Column(name = "commodity")
    @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
    private String commodity;

    @Column(name = "packing_order")
    private String packingOrder;

    @Column(name = "length")
    private BigDecimal length;

    @Column(name = "length_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String lengthUnit;

    @Column(name = "width")
    private BigDecimal width;

    @Column(name = "width_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String widthUnit;

    @Column(name = "height")
    private BigDecimal height;

    @Column(name = "height_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
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
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    private String minTempUnit;

    @Column(name = "max_temp")
    private BigDecimal maxTemp;

    @JoinColumn(name = "max_temp_unit")
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
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
    @MasterData(type = MasterDataType.DG_CLASS)
    private String DGClass;

    @Column(name = "hazardous")
    private Boolean hazardous;

    @Column(name = "commodity_id")
    @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
    private Long commodityId;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;

    @Column(name = "volume_weight")
    private BigDecimal volumeWeight;

    @Column(name = "volume_weight_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeWeightUnit;

    @Column(name = "vin_number")
    private String vinNumber;

    @Column(name = "container_id")
    private Long containerId;

    @Column(name = "transport_mode")
    @MasterData(type = MasterDataType.MODE)
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

    @Column(name = "commodity_group")
    @MasterData(type = MasterDataType.COMMODITY_GROUP)
    private String commodityGroup;

    @Column(name = "is_dimension")
    private Boolean isDimension;

    @Column(name = "is_contract_enforced")
    private Boolean isContractEnforced;

    @Column(name = "handling_info")
    private String handlingInfo;
}

