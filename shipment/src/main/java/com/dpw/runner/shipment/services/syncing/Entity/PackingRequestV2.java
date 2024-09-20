package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class PackingRequestV2 implements IRunnerRequest {
    @JsonProperty("Guid")
    private UUID Guid;
    @JsonProperty("OriginName")
    private String OriginName;
    @JsonProperty("Chargeable")
    private BigDecimal Chargeable;
    @JsonProperty("ChargeableUnit")
    private String ChargeableUnit;
    @JsonProperty("CommodityCode")
    private String CommodityCode;
    @JsonProperty("ConsolidationId")
    private Long ConsolidationId;
    @JsonProperty("ContainerNumber")
    private String ContainerNumber;
    @JsonProperty("CountryCode")
    private String CountryCode;
    @JsonProperty("CustomsReleaseCode")
    private String CustomsReleaseCode;
    @JsonProperty("DGClass")
    private String DGClass;
    @JsonProperty("DGGoodsId")
    private Long DGGoodsId;
    @JsonProperty("DGSubstanceId")
    private Integer DGSubstanceId;
    @JsonProperty("FlashPoint")
    private String FlashPoint;
    @JsonProperty("GoodsDescription")
    private String GoodsDescription;
    @JsonProperty("Hazardous")
    private Boolean Hazardous;
    @JsonProperty("Height")
    private BigDecimal Height;
    @JsonProperty("HeightUnit")
    private String HeightUnit;
    @JsonProperty("HSCode")
    private String HSCode;
    @JsonProperty("InnerPackageNumber")
    private String InnerPackageNumber;
    @JsonProperty("InnerPackageType")
    private String InnerPackageType;
    @JsonProperty("InnerPacksCount")
    private Long InnerPacksCount;
    @JsonProperty("InnerPacksId")
    private Long InnerPacksId;
    @JsonProperty("Inspections")
    private String Inspections;
    @JsonProperty("IsTemperatureControlled")
    private Boolean IsTemperatureControlled;
    @JsonProperty("Length")
    private BigDecimal Length;
    @JsonProperty("LengthUnit")
    private String LengthUnit;
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
    @JsonProperty("NetWeight")
    private BigDecimal NetWeight;
    @JsonProperty("NetWeightUnit")
    private String NetWeightUnit;
    @JsonProperty("Origin")
    private String Origin;
    @JsonProperty("PackingOrder")
    private String PackingOrder;
    @JsonProperty("Packs")
    private String Packs;
    @JsonProperty("PacksType")
    private String PacksType;
    @JsonProperty("ReferenceNumber")
    private String ReferenceNumber;
    @JsonProperty("ShipmentId")
    private Long ShipmentId;
    @JsonProperty("ShipmentNumber")
    private String ShipmentNumber;
    @JsonProperty("TransportMode")
    private String TransportMode;
    @JsonProperty("UNDGContact")
    private String UNDGContact;
    @JsonProperty("VinNumber")
    private String VinNumber;
    @JsonProperty("Volume")
    private BigDecimal Volume;
    @JsonProperty("VolumeUnit")
    private String VolumeUnit;
    @JsonProperty("VolumeWeight")
    private BigDecimal VolumeWeight;
    @JsonProperty("VolumeWeightUnit")
    private String VolumeWeightUnit;
    @JsonProperty("Weight")
    private BigDecimal Weight;
    @JsonProperty("WeightUnit")
    private String WeightUnit;
    @JsonProperty("Width")
    private BigDecimal Width;
    @JsonProperty("WidthUnit")
    private String WidthUnit;
    @JsonProperty("ShipmentGuid")
    private UUID ShipmentGuid;
    @JsonProperty("ConsolidationGuid")
    private UUID ConsolidationGuid;
    @JsonProperty("HandlingInfo")
    private String HandlingInfo;
    @JsonProperty("CommodityGroup")
    private String commodityGroup;
    @JsonProperty("UnNumberAir")
    private String unNumberAir;
    @JsonProperty("DgClassAir")
    private String dgClassAir;
    @JsonProperty("DgClassAirDescription")
    private String dgClassAirDescription;
    @JsonProperty("TenantId")
    private Integer tenantId;
}
