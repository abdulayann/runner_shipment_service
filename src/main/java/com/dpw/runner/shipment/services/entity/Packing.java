package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.*;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;

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
    @DedicatedMasterData(type = Constants.DG_SUBSTANCE)
    private Integer DGSubstanceId;

    @Column(name = "packs")
    private String packs;

    @Column(name = "packs_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsType;

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
    @UnlocationData
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
    @Size(max=255, message = "max size is 255 for goods_description")
    private String goodsDescription;

    @Column(name = "reference_number")
    private String referenceNumber;

    @Column(name = "dg_class_id")
    @MasterData(type = MasterDataType.DG_CLASS)
    private String DGClass;

    @Column(name = "hazardous")
    private Boolean hazardous = false;

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
    @Size(max=2500, message = "max size is 2500 for handling_info")
    private String handlingInfo;

    @Column(name = "contract_enforced_quantity_limit")
    private Long contractEnforcedQuantityLimit;

    @Column(name = "un_number_air")
    @Size(max=31, message = "max size is 31 for un_number_air")
    private String unNumberAir;

    @Column(name = "dg_class_air")
    @Size(max=31, message = "max size is 31 for dg_class_air")
    private String dgClassAir;

    @Column(name = "dg_class_air_description")
    @Size(max=255, message = "max size is 255 for dg_class_air_description")
    private String dgClassAirDescription;

    @Column(name = "date_type")
    @Enumerated(EnumType.STRING)
    private DateBehaviorType dateType;

    @Column(name = "cargo_gate_in_date")
    private LocalDateTime cargoGateInDate;

    @Column(name = "un_number")
    @Size(max=31, message = "max size is 31 for un_number")
    private String unNumber;

    @Column(name = "proper_shipping_name")
    @Size(max=63, message = "max size is 63 for proper_shipping_name")
    private String properShippingName;

    @Column(name = "packing_group")
    @Size(max=31, message = "max size is 31 for packing_group")
    @MasterData(type = MasterDataType.PACKING_GROUP)
    private String packingGroup;

    @Column(name = "minimum_flash_point")
    private BigDecimal minimumFlashPoint;

    @Column(name = "minimum_flash_point_unit")
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    @Size(max = 3, message = "max size is 3 for minimum_flash_point_unit")
    private String minimumFlashPointUnit;

    @Column(name = "marine_pollutant")
    private Boolean marinePollutant = false;

}

