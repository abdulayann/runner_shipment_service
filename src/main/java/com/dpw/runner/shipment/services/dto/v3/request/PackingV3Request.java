package com.dpw.runner.shipment.services.dto.v3.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import io.swagger.annotations.ApiModel;
import lombok.*;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Getter
@Setter
@ApiModel(value = "Packing V3 request model")
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class PackingV3Request extends CommonRequest implements IRunnerRequest {
    private Long id;
    private UUID guid;
    private Long consolidationId;
    private Long shipmentId;
    private Long bookingId;
    private Integer DGGoodsId;
    private Integer DGSubstanceId;
    @NotBlank(message = "packs is required")
    private String packs;
    @NotBlank(message = "Packs type is required")
    private String packsType;
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private String inspections;
    private String origin;
    @NotBlank(message = "Commodity is required")
    private String commodity;
    private String packingOrder;
    private BigDecimal length;
    private String lengthUnit;
    private BigDecimal width;
    private String widthUnit;
    private BigDecimal height;
    private String heightUnit;
    @Size(max = 25000, message = "Max size is 25000 for marks and num")
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
    @Size(max = 25000, message = "Max size is 25000 for goods description")
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
