package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@ApiModel("AWB Packing Info Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbPackingInfo implements Serializable {
    private UUID guid;
    private Integer dgGoodsId;
    private Integer dgSubstanceId;
    private String packs;
    private String packsType;
    private String containerNumber;
    private BigDecimal weight;
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String weightUnit;
    private BigDecimal volume;
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;
    private String inspections;
    @UnlocationData
    private String origin;
    @DedicatedMasterData(type = Constants.COMMODITY_TYPE_MASTER_DATA)
    private String commodity;
    private String packingOrder;
    private BigDecimal length;
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String lengthUnit;
    private BigDecimal width;
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String widthUnit;
    private BigDecimal height;
    @MasterData(type = MasterDataType.DIMENSION_UNIT)
    private String heightUnit;
    private String marksnNums;
    private String flashPoint;
    private String undgContact;
    private Boolean isTemperatureControlled;
    private BigDecimal minTemp;
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    private String minTempUnit;
    private BigDecimal maxTemp;
    @MasterData(type = MasterDataType.TEMPERATURE_UNIT)
    private String maxTempUnit;
    private String hsCode;
    private String countryCode;
    private String goodsDescription;
    private String referenceNumber;
    private String dgClass;
    private Boolean hazardous;
    private BigDecimal netWeight;
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;
    private BigDecimal volumeWeight;
    private String volumeWeightUnit;
    private Integer transportId;
    private String awbNumber;
    private UUID mawbGoodsDescGuid;
    private UUID awbGoodsDescriptionInfoGuid;
    private Long mawbGoodsDescId;
    private String unNumberAir;
    private String dgClassAir;
    private String dgClassAirDescription;
    private Integer tenantId;
}
