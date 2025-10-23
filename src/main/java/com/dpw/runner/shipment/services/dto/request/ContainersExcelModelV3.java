package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.dpw.runner.shipment.services.utils.ExcelCell;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContainersExcelModelV3 {
    @ExcelCell(displayName = "Guid")
    private String guid;

    private Long consolidationId;

    private Long bookingId;

    private Long loggingId;
    @ExcelCell(displayName = "ContainerCode", displayNameOverride = "Type")
    private String containerCode;
    @ExcelCell(displayName = "ContainerNumber", displayNameOverride = "Container No.")
    private String containerNumber;
    @ExcelCell(displayName = "SealNumber",requiredInV3 = false)
    private String sealNumber;
    @ExcelCell(displayName = "DescriptionOfGoods", displayNameOverride = "Description")
    private String descriptionOfGoods;
    @ExcelCell(displayName = "NetWeight", displayNameOverride = "Gr. Wt.")
    private BigDecimal netWeight;
    @ExcelCell(displayName = "NetWeightUnit", displayNameOverride = "Gr. Wt. Unit")
    private String netWeightUnit;
    @ExcelCell(displayName = "GrossWeight", displayNameOverride = "Cargo Wt.")
    private BigDecimal grossWeight;
    @ExcelCell(displayName = "GrossWeightUnit", displayNameOverride = "Cargo Wt. Unit")
    private String grossWeightUnit;
    @ExcelCell(displayName = "Measurement", requiredInV3 = false)
    private BigDecimal measurement;
    @ExcelCell(displayName = "MeasurementUnit", requiredInV3 = false)
    private String measurementUnit;
    @ExcelCell(displayName = "Commodity")
    private String commodityCode;
    @ExcelCell(displayName = "HsCode", displayNameOverride = "HS Code")
    private String hsCode;
    @ExcelCell(displayName = "IsShipperOwned", displayNameOverride = "Shipper Owned Container")
    private Boolean isShipperOwned;
    @ExcelCell(displayName = "IsEmpty", displayNameOverride = "Empty Container")
    private Boolean isEmpty;
    @ExcelCell(displayName = "ContainerCount", requiredInV3 = false)
    private Long containerCount;
    @ExcelCell(displayName = "CarrierSealNumber", displayNameOverride = "Carrier Seal")
    private String carrierSealNumber;
    @ExcelCell(displayName = "ShipperSealNumber", displayNameOverride = "Shipper Seal")
    private String shipperSealNumber;
    @ExcelCell(displayName = "TerminalOperatorSealNumber",requiredInV3 = false)
    private String terminalOperatorSealNumber;
    @ExcelCell(displayName = "VeterinarySealNumber", displayNameOverride = "Veterinary Seal")
    private String veterinarySealNumber;
    @ExcelCell(displayName = "CustomsSealNumber", displayNameOverride = "Customs Seal")
    private String customsSealNumber;
    @ExcelCell(displayName = "CustomsReleaseCode", requiredInV3 = false)
    private String customsReleaseCode;
    @ExcelCell(displayName = "ContainerStuffingLocation", requiredInV3 = false)
    private String containerStuffingLocation;
    @ExcelCell(displayName = "Comments", displayNameOverride = "Container Comments")
    private String containerComments;
    @ExcelCell(displayName = "GrossVolume", displayNameOverride = "Vol.")
    private BigDecimal grossVolume;
    @ExcelCell(displayName = "GrossVolumeUnit", displayNameOverride = "Vol. Unit")
    private String grossVolumeUnit;
    @ExcelCell(displayName = "IsReefer", displayNameOverride = "Is Reefer")
    private Boolean isReefer;
    @ExcelCell(displayName = "Temperature", displayNameOverride = "Min Temp")
    private BigDecimal minTemp;
    @ExcelCell(displayName = "TemperatureUnit", displayNameOverride = "Min Temp Unit")
    private String minTempUnit;
    @ExcelCell(displayName = "MaxTemp",requiredInV3 = false)
    private BigDecimal maxTemp;
    @ExcelCell(displayName = "MaxTempUnit", requiredInV3 = false)
    private String maxTempUnit;
    @ExcelCell(displayName = "HblDeliveryMode", displayNameOverride = "Mode")
    private String hblDeliveryMode;
    @ExcelCell(displayName = "AllocationDate",requiredInV3 = false)
    private LocalDateTime allocationDate;
    @ExcelCell(displayName = "DgClass", displayNameOverride = "DG Class")
    private String dgClass;
    @ExcelCell(displayName = "DangerousGoods", displayNameOverride = "Dangerous Goods")
    private Boolean hazardous;
    @ExcelCell(displayName = "UN#")
    private String hazardousUn;
    @ExcelCell(displayName = "TareWeight", displayNameOverride = "Tare Wt.")
    private BigDecimal tareWeight;
    @ExcelCell(displayName = "TareWeightUnit", displayNameOverride = "Tare Wt. Unit")
    private String tareWeightUnit;
    @ExcelCell(displayName = "SerialNumber", requiredInV3 = false)
    private String serialNumber;
    @ExcelCell(displayName = "InnerPackageNumber", requiredInV3 = false)
    private String innerPackageNumber;
    @ExcelCell(displayName = "InnerPackageType", requiredInV3 = false)
    private String innerPackageType;
    @ExcelCell(displayName = "PackageLength", requiredInV3 = false)
    private BigDecimal packageLength;
    @ExcelCell(displayName = "PackageBreadth", requiredInV3 = false)
    private BigDecimal packageBreadth;
    @ExcelCell(displayName = "PackageHeight", requiredInV3 = false)
    private BigDecimal packageHeight;
    @ExcelCell(displayName = "IsTemperatureMaintained",requiredInV3 = false)
    private Boolean isTemperatureMaintained;
    @ExcelCell(displayName = "Packs", displayNameOverride = "Packages")
    private String packs;
    @ExcelCell(displayName = "PacksType", displayNameOverride = "Packages Unit")
    private String packsType;
    @ExcelCell(displayName = "MarksNums", displayNameOverride = "Marks & Numbers")
    private String marksNums;
    @ExcelCell(displayName = "InnerPackageMeasurementUnit", requiredInV3 = false)
    private String innerPackageMeasurementUnit;
    @ExcelCell(displayName = "PACRNumber", requiredInV3 = false)
    private String pacrNumber;
    @ExcelCell(displayName = "Chargeable",requiredInV3 = false)
    private BigDecimal chargeable;
    @ExcelCell(displayName = "ChargeableUnit",requiredInV3 = false)
    private String chargeableUnit;
    @ExcelCell(displayName = "IsOwnContainer", displayNameOverride = "DP World Container")
    private Boolean isOwnContainer;
    @ExcelCell(displayName = "TransportMode", requiredInV3 = false)
    private String transportMode;
    @ExcelCell(displayName = "Status",requiredInV3 = false)
    private ContainerStatus status;
    @ExcelCell(displayName = "ExtraParams", requiredInV3 = false)
    private String extraParams;
    @ExcelCell(displayName = "Remarks",requiredInV3 = false)
    private String remarks;
    @ExcelCell(displayName = "AllocatedWeight", requiredInV3 = false)
    private BigDecimal allocatedWeight;
    @ExcelCell(displayName = "AllocatedWeightUnit", requiredInV3 = false)
    private String allocatedWeightUnit;
    @ExcelCell(displayName = "AllocatedVolume",requiredInV3 = false)
    private BigDecimal allocatedVolume;
    @ExcelCell(displayName = "AllocatedVolumeUnit",requiredInV3 = false)
    private String allocatedVolumeUnit;
    @ExcelCell(displayName = "AchievedWeight", requiredInV3 = false)
    private BigDecimal achievedWeight;
    @ExcelCell(displayName = "AchievedWeightUnit", requiredInV3 = false)
    private String achievedWeightUnit;
    @ExcelCell(displayName = "AchievedVolume", requiredInV3 = false)
    private BigDecimal achievedVolume;
    @ExcelCell(displayName = "AchievedVolumeUnit", requiredInV3 = false)
    private String achievedVolumeUnit;
    @ExcelCell(displayName = "WeightUtilization", requiredInV3 = false)
    private String weightUtilization;
    @ExcelCell(displayName = "VolumeUtilization", requiredInV3 = false)
    private String volumeUtilization;
    @ExcelCell(displayName = "CommodityGroup", displayNameOverride = "Commodity Category")
    private String commodityGroup;
    @ExcelCell(displayName = "IsContractEnforced", requiredInV3 = false)
    private Boolean isContractEnforced;
    @ExcelCell(displayName = "ContractEnforcedQuantityLimit",requiredInV3 = false)
    private Long contractEnforcedQuantityLimit;
    @ExcelCell(displayName = "OwnType",requiredInV3 = false)
    private String ownType;
    @ExcelCell(displayName = "HandlingInfo", displayNameOverride = "Handling Info")
    private String handlingInfo;
    @ExcelCell(displayName = "IsPart",requiredInV3 = false)
    private Boolean isPart;
    @ExcelCell(displayName = "IsAttached",requiredInV3 = false)
    private Boolean isAttached;
    @ExcelCell(displayName = "ShipmentNumber")
    private String shipmentNumbers;
    @ExcelCell(displayName = "ProperShippingName", displayNameOverride = "Proper Shipping Name")
    private String properShippingName;
    @ExcelCell(displayName = "MarinePollutant", displayNameOverride = "Marine Pollutant")
    private Boolean marinePollutant;
    @ExcelCell(displayName = "PackingGroup", displayNameOverride = "Packing Group")
    private String packingGroup;
    @ExcelCell(displayName = "MinimumFlashPoint", displayNameOverride = "Min. Flash Point")
    private BigDecimal minimumFlashPoint;
    @ExcelCell(displayName = "MinimumFlashPointUnit", displayNameOverride = "Min. Flash Point Unit")
    private String minimumFlashPointUnit;
    @ExcelCell(displayName = "UNNumber", displayNameOverride = "UN Number")
    private String unNumber;
}
