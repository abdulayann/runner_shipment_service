package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

@Data
public class AwbPackingInfoResponse implements IRunnerResponse {
    private UUID guid;
    private Integer dgGoodsId;
    private Integer dgSubstanceId;
    private String packs;
    private String packsType;
    private String containerNumber;
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private String inspections;
    private String origin;
    private String commodity;
    private Long CommodityId;
    private String packingOrder;
    private BigDecimal length;
    private String lengthUnit;
    private BigDecimal width;
    private String widthUnit;
    private BigDecimal height;
    private String heightUnit;
    private String marksnNums;
    private String flashPoint;
    private String undgContact;
    private Boolean isTemperatureControlled;
    private BigDecimal minTemp;
    private String minTempUnit;
    private BigDecimal maxTemp;
    private String maxTempUnit;
    private String hsCode;
    private String countryCode;
    private String goodsDescription;
    private String referenceNumber;
    private String dgClass;
    private Boolean hazardous;
    private Integer nommodityId;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private BigDecimal volumeWeight;
    private String volumeWeightUnit;
    private Integer transportId;
    private String awbNumber;
    private Long mawbGoodsDescId;
    private UUID awbGoodsDescriptionInfoGuid;
    private String unNumberAir;
    private String dgClassAir;
    private String dgClassAirDescription;
    private Integer tenantId;
    //Master Data
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
}
