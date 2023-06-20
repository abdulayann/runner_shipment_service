package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import lombok.*;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "containers")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class Containers extends BaseEntity {

    @Column(name = "consolidation_id")
    private int consolidationId;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "entity_id")
    private String entityId;

    @Column(name = "container_type_id")
    private Long containerTypeId;

    @Column(name = "container_number")
    private String containerNumber;

    @Column(name = "seal_number")
    private String sealNumber;

    @Column(name = "description_of_goods")
    private String descriptionOfGoods;

    @Column(name = "no_of_packages")
    private Long noOfPackages;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    private String netWeightUnit;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    private String grossWeightUnit;

    @Column(name = "measurement")
    private BigDecimal measurement;

    @Column(name = "measurement_unit")
    private String measurementUnit;

    @Column(name = "commodity_id")
    private Long commodityId;

    @Column(name = "hs_code")
    private String hsCode;

    @Column(name = "is_shipper_owned")
    private Boolean isShipperOwned;

    @Column(name = "is_empty")
    private Boolean isEmpty;

    @Column(name = "container_count")
    private Long containerCount;

    @Column(name = "carrier_seal_number")
    private String carrierSealNumber;

    @Column(name = "shipper_seal_number")
    private String shipperSealNumber;

    @Column(name = "terminal_operator_seal_number")
    private String terminalOperatorSealNumber;

    @Column(name = "veterinary_seal_number")
    private String veterinarySealNumber;

    @Column(name = "customs_seal_number")
    private String customsSealNumber;

    @Column(name = "customs_release_code")
    private String customsReleaseCode;

    @Column(name = "container_stuffing_location")
    private Long containerStuffingLocation;

    @Column(name = "container_comments")
    private String containerComments;

    @Column(name = "gross_volume")
    private BigDecimal grossVolume;

    @Column(name = "gross_volume_unit")
    private String grossVolumeUnit;

    @Column(name = "is_reefer")
    private boolean isReefer;

    @Column(name = "min_temp")
    private BigDecimal minTemp;

    @Column(name = "min_temp_unit_id")
    private String minTempUnit;

    @Column(name = "max_temp")
    private BigDecimal maxTemp;

    @Column(name = "max_temp_unit")
    private String maxTempUnit;

    @Column(name = "hbl_delivery_mode")
    private String hblDeliveryMode;

    @Column(name = "per_container_cost_rate")
    private String perContainerCostRate;

    @Column(name = "per_container_sell_rate")
    private String perContainerSellRate;

    @Column(name = "current_cost_rate")
    private BigDecimal currentCostRate;

    @Column(name = "minimum_cost")
    private BigDecimal minimumCost;

    @Column(name = "total_cost_value")
    private BigDecimal totalCostValue;

    @Column(name = "current_sell_rate")
    private BigDecimal currentSellRate;

    @Column(name = "minimum_sell")
    private BigDecimal minimumSell;

    @Column(name = "total_sell_value")
    private BigDecimal totalSellValue;

    @Column(name = "pickup_address")
    private String pickupAddress;

    @Column(name = "delivery_address")
    private String deliveryAddress;

    @Column(name = "allocation_date")
    private LocalDateTime allocationDate;

    @Column(name = "dg_class")
    private Long dgClass;

    @Column(name = "hazardous")
    private Boolean hazardous;

    @Column(name = "hazardous_un")
    private String hazardousUn;

    @Column(name = "tare_weight")
    private BigDecimal tareWeight;

    @Column(name = "tare_weight_unit")
    private String tareWeightUnit;

    @Column(name = "serial_number")
    private String serialNumber;

    @Column(name = "inner_package_number")
    private String innerPackageNumber;

    @Column(name = "inner_package_type")
    private String innerPackageType;

    @Column(name = "package_length")
    private BigDecimal packageLength;

    @Column(name = "package_breadth")
    private BigDecimal packageBreadth;

    @Column(name = "package_height")
    private BigDecimal packageHeight;

    @Column(name = "is_temperature_maintained")
    private boolean isTemperatureMaintained;

    @Column(name = "packs")
    private String packs;

    @Column(name = "packs_type")
    private String packsType;

    @Column(name = "marks_n_nums")
    private String marksNums;

    @Column(name = "inner_package_measurement_unit")
    private String innerPackageMeasurementUnit;

    @Column(name = "pacr_number")
    private String pacrNumber;

    @Column(name = "chargeable")
    private BigDecimal chargeable;

    @Column(name = "chargeable_unit")
    private String chargeableUnit;

    @Column(name = "is_own_container")
    private Boolean isOwnContainer;

    @Column(name = "pack_id")
    private int packId;

    @Column(name = "transport_mode")
    private String transportMode;

    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private ContainerStatus status;

    @Column(name = "extra_params")
    private String extraParams;

    @Column(name = "remarks")
    private String remarks;

    @Column(name = "allocated_weight")
    private BigDecimal allocatedWeight;

    @Column(name = "allocated_weight_unit")
    private String allocatedWeightUnit;

    @Column(name = "allocated_volume")
    private BigDecimal allocatedVolume;

    @Column(name = "allocated_volume_unit_id")
    private String allocatedVolumeUnit;

    @Column(name = "achieved_weight")
    private BigDecimal achievedWeight;

    @Column(name = "achieved_weight_unit")
    private String achievedWeightUnit;

    @Column(name = "achieved_volume")
    private BigDecimal achievedVolume;

    @Column(name = "achieved_volume_unit")
    private String achievedVolumeUnit;

    @Column(name = "weight_utilization")
    private String weightUtilization;

    @Column(name = "volume_utilization")
    private String volumeUtilization;
}
