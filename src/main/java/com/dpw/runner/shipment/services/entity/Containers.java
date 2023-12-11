package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Builder
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

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "logging_id")
    private Long loggingId;

    @Column(name = "container_code")
    @DedicatedMasterData(type = Constants.CONTAINER_TYPE_MASTER_DATA)
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
    @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
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
    @UnlocationData
    private String containerStuffingLocation;

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
    @Size(max=9, message = "max size is 9 for hbl_delivery_mode")
    @MasterData(type = MasterDataType.HBL_DELIVERY_MODE)
    private String hblDeliveryMode;

    @Column(name = "allocation_date")
    private LocalDateTime allocationDate;

    @Column(name = "dg_class")
    @MasterData(type = MasterDataType.DG_CLASS)
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
    @Size(max=100, message = "max size is 100 for serial_number")
    private String serialNumber;

    @Column(name = "inner_package_number")
    @Size(max=100, message = "max size is 100 for inner_package_number")
    private String innerPackageNumber;

    @Column(name = "inner_package_type")
    @Size(max=50, message = "max size is 50 for inner_package_type")
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
    @Size(max=50, message = "max size is 50 for packs")
    private String packs;

    @Column(name = "packs_type")
    @Size(max=50, message = "max size is 50 for packs_type")
    private String packsType;

    @Column(name = "marks_n_nums")
    @Size(max=50, message = "max size is 50 for marks_n_nums")
    private String marksNums;

    @Column(name = "inner_package_measurement_unit")
    @Size(max=50, message = "max size is 50 for inner_package_measurement_unit")
    private String innerPackageMeasurementUnit;

    @Column(name = "pacr_number")
    private String pacrNumber;

    @Column(name = "chargeable")
    private BigDecimal chargeable;

    @Column(name = "chargeable_unit")
    @Size(max=3, message = "max size is 3 for chargeable_unit")
    private String chargeableUnit;

    @Column(name = "is_own_container")
    private Boolean isOwnContainer;

    @Column(name = "transport_mode")
    private String transportMode;

    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private ContainerStatus status;

    @Column(name = "extra_params")
    @Size(max=2000, message = "max size is 2000 for extra_params")
    private String extraParams;

    @Column(name = "remarks")
    @Size(max=1000, message = "max size is 1000 for remarks")
    private String remarks;

    @Column(name = "allocated_weight")
    private BigDecimal allocatedWeight;

    @Column(name = "allocated_weight_unit")
    @Size(max=4, message = "max size is 4 for allocated_weight_unit")
    private String allocatedWeightUnit;

    @Column(name = "allocated_volume")
    private BigDecimal allocatedVolume;

    @Column(name = "allocated_volume_unit_id")
    private String allocatedVolumeUnit;

    @Column(name = "achieved_weight")
    private BigDecimal achievedWeight;

    @Column(name = "achieved_weight_unit")
    @Size(max=4, message = "max size is 4 for achieved_weight_unit")
    private String achievedWeightUnit;

    @Column(name = "achieved_volume")
    private BigDecimal achievedVolume;

    @Column(name = "achieved_volume_unit")
    @Size(max=4, message = "max size is 4 achieved_volume_unit")
    private String achievedVolumeUnit;

    @Column(name = "weight_utilization")
    private String weightUtilization;

    @Column(name = "volume_utilization")
    private String volumeUtilization;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "pickup_address_id", referencedColumnName = "id")
    private Parties pickupAddress;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "delivery_address_id", referencedColumnName = "id")
    private Parties deliveryAddress;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CONTAINERS'")
    private List<Events> eventsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "containerId")
    private List<Packing> packsList;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "shipments_containers_mapping",
            joinColumns = @JoinColumn(name = "container_id"),
            inverseJoinColumns = @JoinColumn(name = "shipment_id"))
    @JsonIgnoreProperties(value = "containersList", allowSetters = true)
    private List<ShipmentDetails> shipmentsList;

    @ManyToMany(fetch = FetchType.LAZY,
            mappedBy = "containersList")
    @JsonIgnore
    private List<BookingCharges> bookingCharges;

    @Column(name = "commodity_group")
    @MasterData(type = MasterDataType.COMMODITY_GROUP)
    private String commodityGroup;

    @Column(name = "is_contract_enforced")
    private Boolean isContractEnforced;

    @Column(name = "contract_enforced_quantity_limit")
    private Long contractEnforcedQuantityLimit;

    @Column(name = "own_type")
    private String ownType;

    @Column(name = "handling_info")
    @Size(max=2500, message = "max size is 2500 for handling_info")
    private String handlingInfo;
}
