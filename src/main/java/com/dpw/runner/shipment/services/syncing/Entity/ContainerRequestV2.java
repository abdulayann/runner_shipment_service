package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
public class ContainerRequestV2 implements IRunnerRequest {

    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("AchievedVolume")
    private BigDecimal AchievedVolume;
    @JsonProperty("AchievedVolumeUnit")
    private String AchievedVolumeUnit;
    @JsonProperty("AchievedWeight")
    private BigDecimal AchievedWeight;
    @JsonProperty("AchievedWeightUnit")
    private String AchievedWeightUnit;
    @JsonProperty("AllocatedVolume")
    private BigDecimal AllocatedVolume;
    @JsonProperty("AllocatedVolumeUnit")
    private String AllocatedVolumeUnit;
    @JsonProperty("AllocatedWeight")
    private BigDecimal AllocatedWeight;
    @JsonProperty("AllocatedWeightUnit")
    private String AllocatedWeightUnit;
    @JsonProperty("AllocationDate")
    private LocalDateTime AllocationDate;
    @JsonProperty("CarrierSealNumber")
    private String CarrierSealNumber;
    @JsonProperty("Chargeable")
    private BigDecimal Chargeable;
    @JsonProperty("ChargeableUnit")
    private String ChargeableUnit;
    @JsonProperty("CommodityCode")
    private String CommodityCode;
    @JsonProperty("ContainerCode")
    private String ContainerCode;
    @JsonProperty("ContainerComments")
    private String ContainerComments;
    @JsonProperty("ContainerCount")
    private String ContainerCount;
    @JsonProperty("ContainerNumber")
    private String ContainerNumber;
    @JsonProperty("ContainerStuffingLocationName")
    private String ContainerStuffingLocationName;
    @JsonProperty("CustomsReleaseCode")
    private String CustomsReleaseCode;
    @JsonProperty("CustomsSealNumber")
    private String CustomsSealNumber;
    @JsonProperty("DeliveryAddressJson")
    private PartyRequestV2 DeliveryAddressJson;
    @JsonProperty("DescriptionOfGoods")
    private String DescriptionOfGoods;
    @JsonProperty("DgClassString")
    private String DgClassString;
    @JsonProperty("EventsList")
    private List<EventsRequestV2> EventsList;
    @JsonProperty("ExtraParams")
    private String ExtraParams;
    @JsonProperty("GrossVolume")
    private BigDecimal GrossVolume;
    @JsonProperty("GrossVolumeUnit")
    private String GrossVolumeUnit;
    @JsonProperty("GrossWeight")
    private BigDecimal GrossWeight;
    @JsonProperty("GrossWeightUnit")
    private String GrossWeightUnit;
    @JsonProperty("IsHazardous")
    private Boolean IsHazardous;
    @JsonProperty("HazardousUn")
    private String HazardousUn;
    @JsonProperty("HblDeliveryMode")
    private String HblDeliveryMode;
    @JsonProperty("HsCode")
    private String HsCode;
    @JsonProperty("InnerPackageMeasurementUnit")
    private String InnerPackageMeasurementUnit;
    @JsonProperty("InnerPackageNumber")
    private String InnerPackageNumber;
    @JsonProperty("InnerPackageType")
    private String InnerPackageType;
    @JsonProperty("IsEmpty")
    private Boolean IsEmpty;
    @JsonProperty("IsOwnContainer")
    private Boolean IsOwnContainer;
    @JsonProperty("IsReefer")
    private Boolean IsReefer;
    @JsonProperty("IsShipperOwned")
    private Boolean IsShipperOwned;
    @JsonProperty("IsTemperatureMaintained")
    private Boolean IsTemperatureMaintained;
    @JsonProperty("MarksnNums")
    private String MarksnNums;
    @JsonProperty("MaxTemp")
    private BigDecimal MaxTemp;
    @JsonProperty("MaxTempUnit")
    private String MaxTempUnit;
    @JsonProperty("MinTemp")
    private BigDecimal MinTemp;
    @JsonProperty("MinTempUnit")
    private String MinTempUnit;
    @JsonProperty("Measurement")
    private BigDecimal Measurement;
    @JsonProperty("MeasurementUnit")
    private String MeasurementUnit;
    @JsonProperty("NetWeight")
    private BigDecimal NetWeight;
    @JsonProperty("NetWeightUnit")
    private String NetWeightUnit;
    @JsonProperty("PackageBreadth")
    private String PackageBreadth;
    @JsonProperty("PackageHeight")
    private String PackageHeight;
    @JsonProperty("PackageLength")
    private String PackageLength;
    @JsonProperty("Packs")
    private String Packs;
    @JsonProperty("PacksList")
    private List<PackingRequestV2> PacksList;  // Different PackRequestV2 was there in create test record model v1
    @JsonProperty("PacksType")
    private String PacksType;
    @JsonProperty("PacrNumber")
    private String PacrNumber;
    @JsonProperty("PickupAddressJson")
    private PartyRequestV2 PickupAddressJson;
    @JsonProperty("Remarks")
    private String Remarks;
    @JsonProperty("SealNumber")
    private String SealNumber;
    @JsonProperty("SerialNumber")
    private String SerialNumber;
//    @JsonProperty("ShipmentsList")
//    private List<Long> ShipmentsList;
    @JsonProperty("ShipperSealNumber")
    private String ShipperSealNumber;
    @JsonProperty("StatusString")
    private String StatusString;
    @JsonProperty("TareWeight")
    private BigDecimal TareWeight;
    @JsonProperty("TareWeightUnit")
    private String TareWeightUnit;
    @JsonProperty("TerminalOperatorSealNumber")
    private String TerminalOperatorSealNumber;
    @JsonProperty("TransportMode")
    private String TransportMode;
    @JsonProperty("VeterinarySealNumber")
    private String VeterinarySealNumber;
    @JsonProperty("VolumeUtilization")
    private String VolumeUtilization;
    @JsonProperty("WeightUtilization")
    private String WeightUtilization;
    @JsonProperty("ShipmentGuids")
    private List<UUID> ShipmentGuids;
    @JsonProperty("ConsolidationGuid")
    private UUID ConsolidationGuid;
    @JsonProperty("HandlingInfo")
    private String handlingInfo;
    @JsonProperty("IsPart")
    private Boolean IsPart;
    @JsonProperty("IsAttached")
    private Boolean IsAttached;
//    @JsonProperty("TruckingDetails") // removing truck from container sync for now
//    private List<TruckDriverDetailsRequestV2> TruckingDetails;
    @JsonProperty("CommodityGroup")
    private String CommodityGroup;
    @JsonProperty("InvoiceNumber")
    private String InvoiceNumber;
    @JsonProperty("InvoiceCurrency")
    private String InvoiceCurrency;
    @JsonProperty("InvoiceValue")
    private BigDecimal InvoiceValue;
}
