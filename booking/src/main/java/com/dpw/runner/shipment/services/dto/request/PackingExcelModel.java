package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.utils.ExcelCell;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PackingExcelModel {
    @ExcelCell(displayName = "Guid")
    private String guid;
    private Long consolidationId;

    private Long shipmentId;

    private Long bookingId;
    @ExcelCell(displayName = "DGGoodsId")
    private Integer DGGoodsId;
    @ExcelCell(displayName = "DGSubstanceId")
    private Integer DGSubstanceId;
    @ExcelCell(displayName = "Packs")
    private String packs;
    @ExcelCell(displayName = "PacksType")
    private String packsType;
    @ExcelCell(displayName = "Weight")
    private BigDecimal weight;
    @ExcelCell(displayName = "WeightUnit")
    private String weightUnit;
    @ExcelCell(displayName = "Volume")
    private BigDecimal volume;
    @ExcelCell(displayName = "VolumeUnit")
    private String volumeUnit;
    @ExcelCell(displayName = "Inspections")
    private String inspections;
    @ExcelCell(displayName = "Origin")
    private String origin;
    @ExcelCell(displayName = "Commodity")
    private String commodity;
    @ExcelCell(displayName = "PackingOrder")
    private String packingOrder;
    @ExcelCell(displayName = "Length")
    private BigDecimal length;
    @ExcelCell(displayName = "LengthUnit")
    private String lengthUnit;
    @ExcelCell(displayName = "Width")
    private BigDecimal width;
    @ExcelCell(displayName = "WidthUnit")
    private String widthUnit;
    @ExcelCell(displayName = "Height")
    private BigDecimal height;
    @ExcelCell(displayName = "HeightUnit")
    private String heightUnit;
    @ExcelCell(displayName = "MarksnNums")
    private String marksnNums;
    @ExcelCell(displayName = "FlashPoint")
    private String flashPoint;
    @ExcelCell(displayName = "UNDGContact")
    private String UNDGContact;
    @ExcelCell(displayName = "IsTemperatureControlled")
    private Boolean isTemperatureControlled;
    @ExcelCell(displayName = "MinTemp")
    private BigDecimal minTemp;
    @ExcelCell(displayName = "MinTempUnit")
    private String minTempUnit;
    @ExcelCell(displayName = "MaxTemp")
    private BigDecimal maxTemp;
    @ExcelCell(displayName = "MaxTempUnit")
    private String maxTempUnit;
    @ExcelCell(displayName = "HSCode")
    private String HSCode;
    @ExcelCell(displayName = "CountryCode")
    private String countryCode;
    @ExcelCell(displayName = "GoodsDescription")
    private String goodsDescription;
    @ExcelCell(displayName = "ReferenceNumber")
    private String referenceNumber;
    @ExcelCell(displayName = "DGClass")
    private String DGClass;
    @ExcelCell(displayName = "DangerousGoods")
    private Boolean hazardous;
    @ExcelCell(displayName = "CommodityId")
    private Long commodityId;
    @ExcelCell(displayName = "NetWeight")
    private BigDecimal netWeight;
    @ExcelCell(displayName = "NetWeightUnit")
    private String netWeightUnit;
    @ExcelCell(displayName = "VolumetricWeight")
    private BigDecimal volumeWeight;
    @ExcelCell(displayName = "VolumetricWeightUnit")
    private String volumeWeightUnit;
    @ExcelCell(displayName = "VinNumber")
    private String vinNumber;
    @ExcelCell(displayName = "ContainerId")
    private Long containerId;
    @ExcelCell(displayName = "TransportMode")
    private String transportMode;
    @ExcelCell(displayName = "InnerPackageNumber")
    private String innerPackageNumber;
    @ExcelCell(displayName = "InnerPackageType")
    private String innerPackageType;
    @ExcelCell(displayName = "Chargeable")
    private BigDecimal chargeable;
    @ExcelCell(displayName = "ChargeableUnit")
    private String chargeableUnit;
    @ExcelCell(displayName = "CustomsReleaseCode")
    private String customsReleaseCode;
    @ExcelCell(displayName = "ShipmentNumber")
    private String shipmentNumber;
    @ExcelCell(displayName = "InnerPacksId")
    private Long innerPacksId;
    @ExcelCell(displayName = "InnerPacksCount")
    private Long innerPacksCount;
    @ExcelCell(displayName = "CommodityGroup")
    private String commodityGroup;
    @ExcelCell(displayName = "IsDimension")
    private Boolean isDimension;
    @ExcelCell(displayName = "IsContractEnforced")
    private Boolean isContractEnforced;
    @ExcelCell(displayName = "HandlingInfo")
    private String handlingInfo;
    @ExcelCell(displayName = "ContractEnforcedQuantityLimit")
    private Long contractEnforcedQuantityLimit;
}
