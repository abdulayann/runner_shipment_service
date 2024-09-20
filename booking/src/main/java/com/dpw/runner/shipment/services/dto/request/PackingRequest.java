package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
@ApiModel(value = "Packing request model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PackingRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long consolidationId;
    private Long shipmentId;
    private Long bookingId;
    private Integer DGGoodsId;
    private Integer DGSubstanceId;
    private String packs;
    private String packsType;
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
    private Boolean hazardous = false;
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
    private String commodityGroup;
    private Boolean isDimension;
    private Boolean isContractEnforced;
    private String handlingInfo;
    private Long contractEnforcedQuantityLimit;
    private String unNumberAir;
    private String dgClassAir;
    private String dgClassAirDescription;
    private DateBehaviorType dateType;
    private LocalDateTime cargoGateInDate;
    private Integer tenantId;
    private String unNumber;
    private String properShippingName;
    private String packingGroup;
    private BigDecimal minimumFlashPoint;
    private String minimumFlashPointUnit;
    private Boolean marinePollutant = false;
}
