package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.ContainerPraStatus;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

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

    @Column(name = "teu")
    private BigDecimal teu;

    @Column(name = "container_number")
    private String containerNumber;

    @Column(name = "seal_number")
    private String sealNumber;

    @Column(name = "description_of_goods")
    @Size(max = 2048, message = "max size is 2048 for description_of_goods")
    private String descriptionOfGoods;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    @Column(name = "measurement")
    private BigDecimal measurement;

    @Column(name = "measurement_unit")
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
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
    @Size(max = 255, message = "max size is 255 for container_comments")
    private String containerComments;

    @Column(name = "gross_volume")
    private BigDecimal grossVolume;

    @Column(name = "gross_volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String grossVolumeUnit;

    @Column(name = "is_reefer")
    private Boolean isReefer;

    @Column(name = "min_temp")
    private BigDecimal minTemp;

    @Column(name = "min_temp_unit_id")
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    private String minTempUnit;

    @Column(name = "max_temp")
    private BigDecimal maxTemp;

    @Column(name = "max_temp_unit")
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
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
    private Boolean hazardous = false;

    @Column(name = "hazardous_un")
    private String hazardousUn;

    @Column(name = "tare_weight")
    private BigDecimal tareWeight;

    @Column(name = "tare_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
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
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String innerPackageMeasurementUnit;

    @Column(name = "pacr_number")
    private String pacrNumber;

    @Column(name = "chargeable")
    private BigDecimal chargeable;

    @Column(name = "chargeable_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
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
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    @Size(max=4, message = "max size is 4 for allocated_weight_unit")
    private String allocatedWeightUnit;

    @Column(name = "allocated_volume")
    private BigDecimal allocatedVolume;

    @Column(name = "allocated_volume_unit_id")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String allocatedVolumeUnit;

    @Column(name = "achieved_weight")
    private BigDecimal achievedWeight;

    @Column(name = "achieved_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    @Size(max=4, message = "max size is 4 for achieved_weight_unit")
    private String achievedWeightUnit;

    @Column(name = "achieved_volume")
    private BigDecimal achievedVolume;

    @Column(name = "achieved_volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
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

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "entity_id")
    @Where(clause = "entity_type = 'CONTAINER'")
    @BatchSize(size = 50)
    private List<Events> eventsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "containerId")
    @BatchSize(size = 50)
    private List<Packing> packsList;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "shipments_containers_mapping",
            joinColumns = @JoinColumn(name = "container_id"),
            inverseJoinColumns = @JoinColumn(name = "shipment_id"))
    @JsonIgnoreProperties(value = "containersList", allowSetters = true)
    @BatchSize(size = 50)
    private Set<ShipmentDetails> shipmentsList;

    @ManyToMany(fetch = FetchType.LAZY,
            mappedBy = "containersList")
    @JsonIgnore
    @BatchSize(size = 50)
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

    @Column(name = "is_part")
    private Boolean isPart;

    @Column(name = "is_attached")
    private Boolean isAttached;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "container_id")
    @BatchSize(size = 50)
    private List<TruckDriverDetails> truckingDetails;

    @Column(name = "invoice_number")
    private String invoiceNumber;

    @Column(name = "invoice_currency")
    private String invoiceCurrency;

    @Column(name = "invoice_value")
    private BigDecimal invoiceValue;

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

    @Column(name = "humidity")
    private BigDecimal humidity;

    @Column(name = "vents")
    private BigDecimal vents;

    @Enumerated(EnumType.STRING)
    @Column(name = "pra_status")
    private ContainerPraStatus praStatus;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Containers that = (Containers) o;
        return Objects.equals(getId(), that.getId()) && Objects.equals(getGuid().toString(), that.getGuid().toString());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId(), getGuid().toString());
    }

}
