package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.ContainerPraStatus;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import io.swagger.annotations.ApiModel;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Set;
import java.util.UUID;
import javax.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ApiModel("Container Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContainerV3Request extends CommonRequest implements IRunnerRequest {

  private Long id;
  private Long consolidationId;
  private Set<Long> shipmentsIds;
  private Long bookingId;
  private Long loggingId;
  private String containerCode;
  private String containerNumber;
  private String sealNumber;
  @Size(max = 2048, message = "max size is 2048 for description_of_goods")
  private String descriptionOfGoods;
  private BigDecimal netWeight;
  private String netWeightUnit;
  private BigDecimal grossWeight;
  private String grossWeightUnit;
  private BigDecimal measurement;
  private String measurementUnit;
  private String commodityCode;
  private String hsCode;
  private Boolean isShipperOwned;
  private Boolean isEmpty;
  private Long containerCount;
  private String carrierSealNumber;
  private String shipperSealNumber;
  private String terminalOperatorSealNumber;
  private String veterinarySealNumber;
  private String customsSealNumber;
  private String customsReleaseCode;
  private String containerStuffingLocation;
  @Size(max = 255, message = "max size is 255 for container_comments")
  private String containerComments;
  private BigDecimal grossVolume;
  private String grossVolumeUnit;
  private Boolean isReefer;
  private BigDecimal minTemp;
  private String minTempUnit;
  private BigDecimal maxTemp;
  private String maxTempUnit;
  @Size(max=9, message = "max size is 9 for hbl_delivery_mode")
  private String hblDeliveryMode;
  private LocalDateTime allocationDate;
  private String dgClass;
  private Boolean hazardous = false;
  private String hazardousUn;
  private BigDecimal tareWeight;
  private String tareWeightUnit;
  @Size(max=100, message = "max size is 100 for serial_number")
  private String serialNumber;
  @Size(max=100, message = "max size is 100 for inner_package_number")
  private String innerPackageNumber;
  @Size(max=50, message = "max size is 50 for inner_package_type")
  private String innerPackageType;
  private BigDecimal packageLength;
  private BigDecimal packageBreadth;
  private BigDecimal packageHeight;
  private Boolean isTemperatureMaintained;
  private String packs;
  @Size(max=50, message = "max size is 50 for packs_type")
  private String packsType;
  @Size(max=50, message = "max size is 50 for marks_n_nums")
  private String marksNums;
  @Size(max=50, message = "max size is 50 for inner_package_measurement_unit")
  private String innerPackageMeasurementUnit;
  private String pacrNumber;
  private BigDecimal chargeable;
  @Size(max=3, message = "max size is 3 for chargeable_unit")
  private String chargeableUnit;
  private Boolean isOwnContainer;
  private String transportMode;
  private ContainerStatus status;
  @Size(max=2000, message = "max size is 2000 for extra_params")
  private String extraParams;
  @Size(max=1000, message = "max size is 1000 for remarks")
  private String remarks;
  private BigDecimal allocatedWeight;
  @Size(max=4, message = "max size is 4 for allocated_weight_unit")
  private String allocatedWeightUnit;
  private BigDecimal allocatedVolume;
  private String allocatedVolumeUnit;
  private BigDecimal achievedWeight;
  @Size(max=4, message = "max size is 4 for achieved_weight_unit")
  private String achievedWeightUnit;
  private BigDecimal achievedVolume;
  @Size(max=4, message = "max size is 4 achieved_volume_unit")
  private String achievedVolumeUnit;
  private String weightUtilization;
  private String volumeUtilization;
  private String commodityGroup;
  private Boolean isContractEnforced;
  private Long contractEnforcedQuantityLimit;
  private String ownType;
  private String handlingInfo;
  private Boolean isPart;
  private Boolean isAttached;
  private String invoiceNumber;
  private String invoiceCurrency;
  private BigDecimal invoiceValue;
  @Size(max=31, message = "max size is 31 for un_number")
  private String unNumber;
  @Size(max=63, message = "max size is 63 for proper_shipping_name")
  private String properShippingName;
  @Size(max=31, message = "max size is 31 for packing_group")
  private String packingGroup;
  private BigDecimal minimumFlashPoint;
  @Size(max = 3, message = "max size is 3 for minimum_flash_point_unit")
  private String minimumFlashPointUnit;
  private Boolean marinePollutant = false;
  private ContainerPraStatus praStatus;
}
