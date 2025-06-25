package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferAllocations implements IEntityTranferBaseEntity {
    private Integer shipmentsCount;
    private Boolean hazardous;
    private LocalDateTime cutoffDate;
    private Boolean isTemperatureControlled;
    private BigDecimal weight;
    private String weightUnit;
    private BigDecimal volume;
    private String volumeUnit;
    private BigDecimal chargable;
    private String chargeableUnit;
    private BigDecimal minTemp;
    private String minTempUnit;
    private BigDecimal maxTemp;
    private String maxTempUnit;
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    private Integer packs;
    private String packsType;
    private Integer dgPacks;
    private String dgPacksType;
    private Integer dgContainerCount;
    private Integer containerCount;
    private BigDecimal teuCount;
    private Map<String, EntityTransferMasterLists> masterData;
}
