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
public class ContainersExcelModel {
    @ExcelCell(displayName = "Guid")
    private String guid;

    private Long consolidationId;

    private Long bookingId;

    private Long loggingId;
    @ExcelCell(displayName = "ContainerCode")
    private String containerCode;
    @ExcelCell(displayName = "ContainerNumber")
    private String containerNumber;
    @ExcelCell(displayName = "SealNumber")
    private String sealNumber;
    @ExcelCell(displayName = "DescriptionOfGoods")
    private String descriptionOfGoods;
    @ExcelCell(displayName = "NetWeight", displayNameOverride = "GrossWeight")
    private BigDecimal netWeight;
    @ExcelCell(displayName = "NetWeightUnit", displayNameOverride = "GrossWeightUnit")
    private String netWeightUnit;
    @ExcelCell(displayName = "GrossWeight", displayNameOverride = "CargoWeight")
    private BigDecimal grossWeight;
    @ExcelCell(displayName = "GrossWeightUnit", displayNameOverride = "CargoWeightUnit")
    private String grossWeightUnit;
    @ExcelCell(displayName = "Measurement", requiredInV3 = false)
    private BigDecimal measurement;
    @ExcelCell(displayName = "MeasurementUnit", requiredInV3 = false)
    private String measurementUnit;
    @ExcelCell(displayName = "Commodity")
    private String commodityCode;
    @ExcelCell(displayName = "HsCode")
    private String hsCode;
    @ExcelCell(displayName = "IsShipperOwned")
    private Boolean isShipperOwned;
    @ExcelCell(displayName = "IsEmpty")
    private Boolean isEmpty;
    @ExcelCell(displayName = "ContainerCount", requiredInV3 = false)
    private Long containerCount;
    @ExcelCell(displayName = "CarrierSealNumber")
    private String carrierSealNumber;
    @ExcelCell(displayName = "ShipperSealNumber")
    private String shipperSealNumber;
    @ExcelCell(displayName = "TerminalOperatorSealNumber")
    private String terminalOperatorSealNumber;
    @ExcelCell(displayName = "VeterinarySealNumber")
    private String veterinarySealNumber;
    @ExcelCell(displayName = "CustomsSealNumber")
    private String customsSealNumber;
    @ExcelCell(displayName = "CustomsReleaseCode", requiredInV3 = false)
    private String customsReleaseCode;
    @ExcelCell(displayName = "ContainerStuffingLocation", requiredInV3 = false)
    private String containerStuffingLocation;
    @ExcelCell(displayName = "Comments")
    private String containerComments;
    @ExcelCell(displayName = "GrossVolume")
    private BigDecimal grossVolume;
    @ExcelCell(displayName = "GrossVolumeUnit")
    private String grossVolumeUnit;
    @ExcelCell(displayName = "IsReefer")
    private Boolean isReefer;
    @ExcelCell(displayName = "Temperature")
    private BigDecimal minTemp;
    @ExcelCell(displayName = "TemperatureUnit")
    private String minTempUnit;
    @ExcelCell(displayName = "MaxTemp")
    private BigDecimal maxTemp;
    @ExcelCell(displayName = "MaxTempUnit", requiredInV3 = false)
    private String maxTempUnit;
    @ExcelCell(displayName = "HblDeliveryMode")
    private String hblDeliveryMode;
    @ExcelCell(displayName = "AllocationDate")
    private LocalDateTime allocationDate;
    @ExcelCell(displayName = "DgClass")
    private String dgClass;
    @ExcelCell(displayName = "DangerousGoods")
    private Boolean hazardous;
    @ExcelCell(displayName = "UN#")
    private String hazardousUn;
    @ExcelCell(displayName = "TareWeight")
    private BigDecimal tareWeight;
    @ExcelCell(displayName = "TareWeightUnit")
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
    @ExcelCell(displayName = "IsTemperatureMaintained")
    private Boolean isTemperatureMaintained;
    @ExcelCell(displayName = "Packs")
    private String packs;
    @ExcelCell(displayName = "PacksType")
    private String packsType;
    @ExcelCell(displayName = "MarksNums")
    private String marksNums;
    @ExcelCell(displayName = "InnerPackageMeasurementUnit", requiredInV3 = false)
    private String innerPackageMeasurementUnit;
    @ExcelCell(displayName = "PACRNumber", requiredInV3 = false)
    private String pacrNumber;
    @ExcelCell(displayName = "Chargeable")
    private BigDecimal chargeable;
    @ExcelCell(displayName = "ChargeableUnit")
    private String chargeableUnit;
    @ExcelCell(displayName = "IsOwnContainer")
    private Boolean isOwnContainer;
    @ExcelCell(displayName = "TransportMode", requiredInV3 = false)
    private String transportMode;
    @ExcelCell(displayName = "Status")
    private ContainerStatus status;
    @ExcelCell(displayName = "ExtraParams", requiredInV3 = false)
    private String extraParams;
    @ExcelCell(displayName = "Remarks")
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
    @ExcelCell(displayName = "CommodityGroup")
    private String commodityGroup;
    @ExcelCell(displayName = "IsContractEnforced", requiredInV3 = false)
    private Boolean isContractEnforced;
    @ExcelCell(displayName = "ContractEnforcedQuantityLimit")
    private Long contractEnforcedQuantityLimit;
    @ExcelCell(displayName = "OwnType")
    private String ownType;
    @ExcelCell(displayName = "HandlingInfo")
    private String handlingInfo;
    @ExcelCell(displayName = "IsPart")
    private Boolean isPart;
    @ExcelCell(displayName = "IsAttached")
    private Boolean isAttached;
    @ExcelCell(displayName = "ShipmentNumber")
    private String shipmentNumbers;
    @ExcelCell(displayName = "ProperShippingName")
    private String properShippingName;
    @ExcelCell(displayName = "MarinePollutant")
    private Boolean marinePollutant;
    @ExcelCell(displayName = "PackingGroup")
    private String packingGroup;
    @ExcelCell(displayName = "MinimumFlashPoint")
    private BigDecimal minimumFlashPoint;
    @ExcelCell(displayName = "MinimumFlashPointUnit")
    private String minimumFlashPointUnit;
    @ExcelCell(displayName = "UNNumber")
    private String unNumber;
}
