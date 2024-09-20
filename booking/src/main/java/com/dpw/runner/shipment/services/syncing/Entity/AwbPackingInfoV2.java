package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AwbPackingInfoV2 implements IRunnerRequest {
    public Integer dgGoodsId;
    public Integer dgSubstanceId;
    public String packs;
    public String packsType;
    public String containerNumber;
    public BigDecimal weight;
    public String weightUnit;
    public BigDecimal volume;
    public String volumeUnit;
    public String inspections;
    public String origin;
    public String commodity;
    public Long CommodityId;
    public String packingOrder;
    public BigDecimal length;
    public String lengthUnit;
    public BigDecimal width;
    public String widthUnit;
    public BigDecimal height;
    public String heightUnit;
    public String marksnNums;
    public String flashPoint;
    public String undgContact;
    public Boolean isTemperatureControlled;
    public BigDecimal minTemp;
    public String minTempUnit;
    public String maxTemp;
    public String maxTempUnit;
    public String hsCode;
    public String countryCode;
    public String goodsDescription;
    public String referenceNumber;
    public String dgClass;
    public Boolean hazardous;
    public Integer nommodityId;// commodity id ?
    public BigDecimal netWeight;
    public String netWeightUnit;
    public BigDecimal volumeWeight;
    public String volumeWeightUnit;
    public Integer transportId;
    public String awbNumber;
    public Long mawbGoodsDescId;
    private UUID awbGoodsDescriptionInfoGuid;
}
