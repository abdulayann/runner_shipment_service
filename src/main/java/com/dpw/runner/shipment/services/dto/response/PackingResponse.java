package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomLocalDateTimeSerializer;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@ApiModel(value = "Packing response model")
public class PackingResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long consolidationId;
    private Long shipmentId;
    private Long bookingId;
    private Integer DGGoodsId;
    private Integer DGSubstanceId;
    private String packs;
    private String packsType;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weight;
    private String weightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
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
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal netWeight;
    private String netWeightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal volumeWeight;
    private String volumeWeightUnit;
    private String vinNumber;
    private Long containerId;
    private String containerNumber;
    private String transportMode;
    private String innerPackageNumber;
    private String innerPackageType;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargeable;
    private String chargeableUnit;
    private String customsReleaseCode;
    private String shipmentNumber;
    private Boolean shipmentHazardous;
    private Long innerPacksId;
    private Long innerPacksCount;
    private String commodityGroup;
    private Map<String, String> unlocationData;
    private Map<String, String> masterData;
    private Map<String, String> commodityTypeData;
    private Boolean isDimension;
    private Boolean isContractEnforced;
    private String handlingInfo;
    private Long contractEnforcedQuantityLimit;
    private String containerDesc;
    private String unNumberAir;
    private String dgClassAir;
    private String dgClassAirDescription;
    private Boolean assigned;
    private DateBehaviorType dateType;
    @JsonSerialize(using = CustomLocalDateTimeSerializer.class)
    private LocalDateTime cargoGateInDate;
    private Integer tenantId;
    private String unNumber;
    private String properShippingName;
    private String packingGroup;
    private BigDecimal minimumFlashPoint;
    private String minimumFlashPointUnit;
    private Boolean marinePollutant;

    public boolean getAssigned() {
        return containerId != null;
    }
}

