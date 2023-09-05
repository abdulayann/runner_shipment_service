package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class PackingRequestV2 {

    private String OriginName;
    private BigDecimal Chargeable;
    private String ChargeableUnit;
    private String CommodityCode;
    private Long CommodityId;
    private Long ConsolidationId;
    private String ContainerNumber;
    private Long ContainerId;
    private String CountryCode;
    private String CustomsReleaseCode;
    private String Dgclass;
    private Long DggoodsId;
    private Long DgsubstanceId;
    private String FlashPoint;
    private String GoodsDescription;
    private Boolean Hazardous;
    private BigDecimal Height;
    private String HeightUnit;
    private String HsCode;
    private String InnerPackageNumber;
    private String InnerPackageType;
    private Long InnerPacksCount;
    private Long InnerPacksId;
    private String Inspections;
    private Boolean IsTemperatureControlled;
    private BigDecimal Length;
    private String LengthUnit;
    private String MarksnNums;
    private BigDecimal MaxTemp;
    private String MaxTempUnit;
    private BigDecimal MinTemp;
    private String MinTempUnit;
    private BigDecimal NetWeight;
    private String NetWeightUnit;
    private String Origin;
    private String PackingOrder;
    private String Packs;
    private String PacksType;
    private String ReferenceNumber;
    private Long ShipmentId;
    private String ShipmentNumber;
    private String TransportMode;
    private String Undgcontact;
    private String VinNumber;
    private BigDecimal Volume;
    private String VolumeUnit;
    private BigDecimal VolumeWeight;
    private String VolumeWeightUnit;
    private BigDecimal Weight;
    private String WeightUnit;
    private BigDecimal Width;
    private String WidthUnit;
}
