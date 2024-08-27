package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferAllocations implements IEntityTranferBaseEntity {
    private UUID guid;
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
    private Map<String, EntityTransferMasterLists> masterData;
}
