package com.dpw.runner.shipment.services.ReportingService.Models.Commons;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class ShipmentContainers {
    public String ContainerNumber;
    public String SealNumber;
    public Long NoofPackages;
    public Long ShipmentPacks;
    public String ShipmentPacksUnit;
    public BigDecimal GrossWeight;
    public String GrossWeightUnit;
    public BigDecimal TareWeight;
    public String TareWeightUnit;
    public BigDecimal Measurement;
    public String MeasurementUnit;
    public BigDecimal GrossVolume;
    public String GrossVolumeUnit;
    public String ContainerTypeCode;
    public BigDecimal ContainerTypeTeu;
    public Long ContainerCount;
    public String ShipmentMarksnNums;
    public BigDecimal NetWeight;
    public String NetWeightUnit;
    public BigDecimal MinTemp;
    public String MinTempUnit;
    public String ShipmentHblDeliveryMode;
    public String DescriptionOfGoods;
    public String CarrierSealNumber;
    public String CustomsSealNumber;
    public String ShipperSealNumber;
    public String BL_ContainerType;
    public String BL_SealNumber;
    public BigDecimal BL_GrossWeight;
    public String BL_GrossWeightUnit;
    public BigDecimal BL_GrossVolume;
    public String BL_GrossVolumeUnit;
    public Long BL_NoofPackages;
    public String BL_CarrierSealNumber;
    public String BL_ContainerNumber;
    public String BL_ContainerDescription;
    public String BL_PackageUnit;
    public String BL_CustomSealNumber;
}
