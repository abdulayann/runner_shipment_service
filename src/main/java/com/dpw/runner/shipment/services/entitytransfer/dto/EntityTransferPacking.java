package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferPacking implements IEntityTranferBaseEntity {
    private UUID guid;
    private UUID containerGuid;
    private Integer DGGoodsId;
    private Integer DGSubstanceId;
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
    private String packingOrder;
    private BigDecimal length;
    private String lengthUnit;
    private BigDecimal width;
    private String widthUnit;
    private BigDecimal height;
    private String heightUnit;
    private String marksnNums;
    private String flashPoint;
    private String UNDGContact;
    private Boolean isTemperatureControlled;
    private BigDecimal minTemp;
    private String minTempUnit;
    private BigDecimal maxTemp;
    private String maxTempUnit;
    private String HSCode;
    private String countryCode;
    private String goodsDescription;
    private String referenceNumber;
    private String DGClass;
    private Boolean hazardous;
    private Long commodityId;
    private BigDecimal netWeight;
    private String netWeightUnit;
    private BigDecimal volumeWeight;
    private String volumeWeightUnit;
    private String vinNumber;
    private Long containerId;
    private String transportMode;
    private String innerPackageNumber;
    private String innerPackageType;
    private BigDecimal chargeable;
    private String chargeableUnit;
    private String customsReleaseCode;
    private String shipmentNumber;
    private Long innerPacksId;
    private Long innerPacksCount;
    private Map<String, EntityTransferMasterLists> masterData;
    private Map<String, EntityTransferCommodityType> commodityTypeMasterData;
}
