package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.dpw.runner.shipment.services.config.LocalDateTimeWithTimeZoneSerializer;
import com.dpw.runner.shipment.services.entity.enums.ContainerStatus;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ContainerModel implements Serializable {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("Guid")
    private UUID guid;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("BookingId")
    private Long bookingId;
    @JsonProperty("LoggingId")
    private Long loggingId;
    @JsonProperty("ContainerCode")
    private String containerCode;
    @JsonProperty("ContainerNumber")
    private String containerNumber;
    @JsonProperty("SealNumber")
    private String sealNumber;
    @JsonProperty("DescriptionOfGoods")
    private String descriptionOfGoods;
    @JsonProperty("NetWeight")
    private BigDecimal netWeight;
    @JsonProperty("NetWeightUnit")
    private String netWeightUnit;
    @JsonProperty("GrossWeight")
    private BigDecimal grossWeight;
    @JsonProperty("GrossWeightUnit")
    private String grossWeightUnit;
    @JsonProperty("Measurement")
    private BigDecimal measurement;
    @JsonProperty("MeasurementUnit")
    private String measurementUnit;
    @JsonProperty("CommodityCode")
    private String commodityCode;
    @JsonProperty("CommodityGroup")
    private String commodityGroup;
    @JsonProperty("HsCode")
    private String hsCode;
    @JsonProperty("IsShipperOwned")
    private Boolean isShipperOwned;
    @JsonProperty("IsEmpty")
    private Boolean isEmpty;
    @JsonProperty("ContainerCount")
    private Long containerCount;
    @JsonProperty("CarrierSealNumber")
    private String carrierSealNumber;
    @JsonProperty("ShipperSealNumber")
    private String shipperSealNumber;
    @JsonProperty("TerminalOperatorSealNumber")
    private String terminalOperatorSealNumber;
    @JsonProperty("VeterinarySealNumber")
    private String veterinarySealNumber;
    @JsonProperty("CustomsSealNumber")
    private String customsSealNumber;
    @JsonProperty("CustomsReleaseCode")
    private String customsReleaseCode;
    @JsonProperty("ContainerStuffingLocation")
    private String containerStuffingLocation;
    @JsonProperty("ContainerComments")
    private String containerComments;
    @JsonProperty("GrossVolume")
    private BigDecimal grossVolume;
    @JsonProperty("GrossVolumeUnit")
    private String grossVolumeUnit;
    @JsonProperty("IsReefer")
    private Boolean isReefer;
    @JsonProperty("MinTemp")
    private BigDecimal minTemp;
    @JsonProperty("MinTempUnit")
    private String minTempUnit;
    @JsonProperty("MaxTemp")
    private BigDecimal maxTemp;
    @JsonProperty("MaxTempUnit")
    private String maxTempUnit;
    @JsonProperty("HblDeliveryMode")
    private String hblDeliveryMode;
    @JsonProperty("AllocationDate")
    @JsonSerialize(using = LocalDateTimeWithTimeZoneSerializer.class)
    private LocalDateTime allocationDate;
    @JsonProperty("DgClass")
    private String dgClass;
    @JsonProperty("Hazardous")
    private Boolean hazardous;
    @JsonProperty("HazardousUn")
    private String hazardousUn;
    @JsonProperty("TareWeight")
    private BigDecimal tareWeight;
    @JsonProperty("TareWeightUnit")
    private String tareWeightUnit;
    @JsonProperty("SerialNumber")
    private String serialNumber;
    @JsonProperty("InnerPackageNumber")
    private String innerPackageNumber;
    @JsonProperty("InnerPackageType")
    private String innerPackageType;
    @JsonProperty("PackageLength")
    private BigDecimal packageLength;
    @JsonProperty("PackageBreadth")
    private BigDecimal packageBreadth;
    @JsonProperty("PackageHeight")
    private BigDecimal packageHeight;
    @JsonProperty("IsTemperatureMaintained")
    private Boolean isTemperatureMaintained;
    @JsonProperty("Packs")
    private String packs;
    @JsonProperty("PacksType")
    private String packsType;
    @JsonProperty("MarksNums")
    private String marksNums;
    @JsonProperty("InnerPackageMeasurementUnit")
    private String innerPackageMeasurementUnit;
    @JsonProperty("PacrNumber")
    private String pacrNumber;
    @JsonProperty("Chargeable")
    private BigDecimal chargeable;
    @JsonProperty("ChargeableUnit")
    private String chargeableUnit;
    @JsonProperty("IsOwnContainer")
    private Boolean isOwnContainer;
    @JsonProperty("TransportMode")
    private String transportMode;
    @JsonProperty("Status")
    private ContainerStatus status;
    @JsonProperty("ExtraParams")
    private String extraParams;
    @JsonProperty("Remarks")
    private String remarks;
    @JsonProperty("AllocatedWeight")
    private BigDecimal allocatedWeight;
    @JsonProperty("AllocatedWeightUnit")
    private String allocatedWeightUnit;
    @JsonProperty("AllocatedVolume")
    private BigDecimal allocatedVolume;
    @JsonProperty("AllocatedVolumeUnit")
    private String allocatedVolumeUnit;
    @JsonProperty("AchievedWeight")
    private BigDecimal achievedWeight;
    @JsonProperty("AchievedWeightUnit")
    private String achievedWeightUnit;
    @JsonProperty("AchievedVolume")
    private BigDecimal achievedVolume;
    @JsonProperty("AchievedVolumeUnit")
    private String achievedVolumeUnit;
    @JsonProperty("WeightUtilization")
    private String weightUtilization;
    @JsonProperty("VolumeUtilization")
    private String volumeUtilization;
    @JsonProperty("PickupAddress")
    private PartiesModel pickupAddress;
    @JsonProperty("DeliveryAddress")
    private PartiesModel deliveryAddress;
    @JsonProperty("EventsList")
    private List<EventsModel> eventsList;
    @JsonProperty("PacksList")
    private List<PackingModel> packsList;
    @JsonProperty("UnNumber")
    private String unNumber;
    @JsonProperty("ProperShippingName")
    private String properShippingName;
    @JsonProperty("PackingGroup")
    private String packingGroup;
    @JsonProperty("MinimumFlashPoint")
    private BigDecimal minimumFlashPoint;
    @JsonProperty("MinimumFlashPointUnit")
    private String minimumFlashPointUnit;
    @JsonProperty("MarinePollutant")
    private Boolean marinePollutant;
}
