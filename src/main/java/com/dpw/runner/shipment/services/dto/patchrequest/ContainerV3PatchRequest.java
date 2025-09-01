package com.dpw.runner.shipment.services.dto.patchrequest;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.ContainerPraStatus;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import lombok.*;
import org.openapitools.jackson.nullable.JsonNullable;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContainerV3PatchRequest extends CommonRequest implements IRunnerRequest {
    private JsonNullable<Long> id;
    private JsonNullable<Long> consolidationId;
    private JsonNullable<Long> shipmentId;
    private JsonNullable<Long> bookingId;
    private JsonNullable<Long> loggingId;
    private JsonNullable<String> containerCode;
    private JsonNullable<String> containerNumber;
    private JsonNullable<String> sealNumber;
    @Size(max = 2048, message = "max size is 2048 for description_of_goods")
    private JsonNullable<String> descriptionOfGoods;
    private JsonNullable<BigDecimal> netWeight;
    private JsonNullable<String> netWeightUnit;
    private JsonNullable<BigDecimal> grossWeight;
    private JsonNullable<String> grossWeightUnit;
    private JsonNullable<BigDecimal> measurement;
    private JsonNullable<String> measurementUnit;
    private JsonNullable<String> commodityCode;
    private JsonNullable<String> hsCode;
    private JsonNullable<Boolean> isShipperOwned;
    private JsonNullable<Boolean> isEmpty;
    @Max(value = 1, message = "Container count cannot be more than 1")
    @Min(value = 1, message = "Container count cannot be less than 1")
    private JsonNullable<Long> containerCount;
    @Size(max = 100, message = "max size is 100 for carrier_seal_number")
    private JsonNullable<String> carrierSealNumber;
    @Size(max = 100, message = "max size is 100 for shipper_seal_number")
    private JsonNullable<String> shipperSealNumber;
    private JsonNullable<String> terminalOperatorSealNumber;
    @Size(max = 100, message = "max size is 100 for veterinary_seal_number")
    private JsonNullable<String> veterinarySealNumber;
    @Size(max = 100, message = "max size is 100 for customs_seal_number")
    private JsonNullable<String> customsSealNumber;
    private JsonNullable<String> customsReleaseCode;
    private JsonNullable<String> containerStuffingLocation;
    @Size(max = 255, message = "max size is 255 for container_comments")
    private JsonNullable<String> containerComments;
    private JsonNullable<BigDecimal> grossVolume;
    private JsonNullable<String> grossVolumeUnit;
    private JsonNullable<Boolean> isReefer;
    private JsonNullable<BigDecimal> minTemp;
    private JsonNullable<String> minTempUnit;
    private JsonNullable<BigDecimal> maxTemp;
    private JsonNullable<String> maxTempUnit;
    @Size(max=9, message = "max size is 9 for hbl_delivery_mode")
    private JsonNullable<String> hblDeliveryMode;
    private JsonNullable<LocalDateTime> allocationDate;
    private JsonNullable<String> dgClass;
    private JsonNullable<Boolean> hazardous;
    private JsonNullable<String> hazardousUn;
    private JsonNullable<BigDecimal> tareWeight;
    private JsonNullable<String> tareWeightUnit;
    @Size(max=100, message = "max size is 100 for serial_number")
    private JsonNullable<String> serialNumber;
    @Size(max=100, message = "max size is 100 for inner_package_number")
    private JsonNullable<String> innerPackageNumber;
    @Size(max=50, message = "max size is 50 for inner_package_type")
    private JsonNullable<String> innerPackageType;
    private JsonNullable<BigDecimal> packageLength;
    private JsonNullable<BigDecimal> packageBreadth;
    private JsonNullable<BigDecimal> packageHeight;
    private JsonNullable<Boolean> isTemperatureMaintained;
    private JsonNullable<String> packs;
    @Size(max=50, message = "max size is 50 for packs_type")
    private JsonNullable<String> packsType;
    @Size(max=50, message = "max size is 50 for marks_n_nums")
    private JsonNullable<String> marksNums;
    @Size(max=50, message = "max size is 50 for inner_package_measurement_unit")
    private JsonNullable<String> innerPackageMeasurementUnit;
    private JsonNullable<String> pacrNumber;
    private JsonNullable<BigDecimal> chargeable;
    @Size(max=3, message = "max size is 3 for chargeable_unit")
    private JsonNullable<String> chargeableUnit;
    private JsonNullable<Boolean> isOwnContainer;
    private JsonNullable<String> transportMode;
    private JsonNullable<ContainerStatus> status;
    @Size(max=2000, message = "max size is 2000 for extra_params")
    private JsonNullable<String> extraParams;
    @Size(max=1000, message = "max size is 1000 for remarks")
    private JsonNullable<String> remarks;
    private JsonNullable<BigDecimal> allocatedWeight;
    @Size(max=4, message = "max size is 4 for allocated_weight_unit")
    private JsonNullable<String> allocatedWeightUnit;
    private JsonNullable<BigDecimal> allocatedVolume;
    private JsonNullable<String> allocatedVolumeUnit;
    private JsonNullable<BigDecimal> achievedWeight;
    @Size(max=4, message = "max size is 4 for achieved_weight_unit")
    private JsonNullable<String> achievedWeightUnit;
    private JsonNullable<BigDecimal> achievedVolume;
    @Size(max=4, message = "max size is 4 achieved_volume_unit")
    private JsonNullable<String> achievedVolumeUnit;
    private JsonNullable<String> weightUtilization;
    private JsonNullable<String> volumeUtilization;
    private JsonNullable<String> commodityGroup;
    private JsonNullable<Boolean> isContractEnforced;
    private JsonNullable<Long> contractEnforcedQuantityLimit;
    private JsonNullable<String> ownType;
    private JsonNullable<String> handlingInfo;
    private JsonNullable<Boolean> isPart;
    private JsonNullable<Boolean> isAttached;
    private JsonNullable<String> invoiceNumber;
    private JsonNullable<String> invoiceCurrency;
    private JsonNullable<BigDecimal> invoiceValue;
    @Size(max=31, message = "max size is 31 for un_number")
    private JsonNullable<String> unNumber;
    @Size(max=63, message = "max size is 63 for proper_shipping_name")
    private JsonNullable<String> properShippingName;
    @Size(max=31, message = "max size is 31 for packing_group")
    private JsonNullable<String> packingGroup;
    private JsonNullable<BigDecimal> minimumFlashPoint;
    @Size(max = 3, message = "max size is 3 for minimum_flash_point_unit")
    private JsonNullable<String> minimumFlashPointUnit;
    private JsonNullable<Boolean> marinePollutant;
    private JsonNullable<ContainerPraStatus> praStatus;
    private JsonNullable<Boolean> openForAttachment;
    private JsonNullable<BigDecimal> humidity;
    private JsonNullable<BigDecimal> vents;
    private JsonNullable<BigDecimal> teu;
    private JsonNullable<Long> packagesPerContainer;
    private JsonNullable<String> containerPackageType;
    private JsonNullable<BigDecimal> cargoWeightPerContainer;
    private JsonNullable<String> containerWeightUnit;
}
