package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "containers")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE containers SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Containers extends MultiTenancy {

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "logging_id")
    private Long loggingId;

    @Column(name = "container_code")
    private String containerCode;

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

    @Column(name = "commodity_code")
    private String commodityCode;

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
    private Boolean isReefer;

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

    @Column(name = "allocation_date")
    private LocalDateTime allocationDate;

    @Column(name = "dg_class")
    private String dgClass;

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
    private Boolean isTemperatureMaintained;

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

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "pickup_address_id", referencedColumnName = "id")
    private Parties pickupAddress;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "delivery_address_id", referencedColumnName = "id")
    private Parties deliveryAddress;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;
}
