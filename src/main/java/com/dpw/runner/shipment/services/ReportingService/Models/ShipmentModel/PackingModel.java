package com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class PackingModel implements Serializable {
    @JsonProperty("Id")
    private Long id;
    @JsonProperty("ConsolidationId")
    private Long consolidationId;
    @JsonProperty("ShipmentId")
    private Long shipmentId;
    @JsonProperty("BookingId")
    private Long bookingId;
    @JsonProperty("DGGoodsId")
    private Integer DGGoodsId;
    @JsonProperty("DGSubstanceId")
    private Integer DGSubstanceId;
    @JsonProperty("Packs")
    private String packs;
    @JsonProperty("PacksType")
    private String packsType;
    @JsonProperty("ContainerNumber")
    private String containerNumber;
    @JsonProperty("Weight")
    private BigDecimal weight;
    @JsonProperty("WeightUnit")
    private String weightUnit;
    @JsonProperty("Volume")
    private BigDecimal volume;
    @JsonProperty("VolumeUnit")
    private String volumeUnit;
    @JsonProperty("Inspections")
    private String inspections;
    @JsonProperty("Origin")
    private String origin;
    @JsonProperty("Commodity")
    private String commodity;
    @JsonProperty("CommodityGroup")
    private String commodityGroup;
    @JsonProperty("PackingOrder")
    private String packingOrder;
    @JsonProperty("Length")
    private BigDecimal length;
    @JsonProperty("LengthUnit")
    private String lengthUnit;
    @JsonProperty("Width")
    private BigDecimal width;
    @JsonProperty("WidthUnit")
    private String widthUnit;
    @JsonProperty("Height")
    private BigDecimal height;
    @JsonProperty("HeightUnit")
    private String heightUnit;
    @JsonProperty("MarksnNums")
    private String marksnNums;
    @JsonProperty("FlashPoint")
    private String flashPoint;
    @JsonProperty("UNDGContact")
    private String UNDGContact;
    @JsonProperty("IsTemperatureControlled")
    private Boolean isTemperatureControlled;
    @JsonProperty("MinTemp")
    private BigDecimal minTemp;
    @JsonProperty("MinTempUnit")
    private String minTempUnit;
    @JsonProperty("MaxTemp")
    private BigDecimal maxTemp;
    @JsonProperty("MaxTempUnit")
    private String maxTempUnit;
    @JsonProperty("HSCode")
    private String HSCode;
    @JsonProperty("CountryCode")
    private String countryCode;
    @JsonProperty("GoodsDescription")
    private String goodsDescription;
    @JsonProperty("ReferenceNumber")
    private String referenceNumber;
    @JsonProperty("DGClass")
    private String DGClass;
    @JsonProperty("Hazardous")
    private Boolean hazardous;
    @JsonProperty("CommodityId")
    private Long commodityId;
    @JsonProperty("NetWeight")
    private BigDecimal netWeight;
    @JsonProperty("NetWeightUnit")
    private String netWeightUnit;
    @JsonProperty("VolumeWeight")
    private BigDecimal volumeWeight;
    @JsonProperty("VolumeWeightUnit")
    private String volumeWeightUnit;
    @JsonProperty("VinNumber")
    private String vinNumber;
    @JsonProperty("ContainerId")
    private Long containerId;
    @JsonProperty("TransportMode")
    private String transportMode;
    @JsonProperty("InnerPackageNumber")
    private String innerPackageNumber;
    @JsonProperty("InnerPackageType")
    private String innerPackageType;
    @JsonProperty("Chargeable")
    private BigDecimal chargeable;
    @JsonProperty("ChargeableUnit")
    private String chargeableUnit;
    @JsonProperty("CustomsReleaseCode")
    private String customsReleaseCode;
    @JsonProperty("ShipmentNumber")
    private String shipmentNumber;
    @JsonProperty("InnerPacksId")
    private Long innerPacksId;
    @JsonProperty("InnerPacksCount")
    private Long innerPacksCount;
    @JsonProperty("UnNumberAir")
    private String unNumberAir;
    @JsonProperty("DgClassAir")
    private String dgClassAir;
    @JsonProperty("DgClassAirDescription")
    private String dgClassAirDescription;
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
