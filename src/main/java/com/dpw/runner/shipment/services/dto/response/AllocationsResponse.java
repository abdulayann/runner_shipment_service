package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomVolumeValueSerializer;
import com.dpw.runner.shipment.services.config.CustomWeightValueSerializer;
import com.dpw.runner.shipment.services.config.DecimalPlaceValueSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@ApiModel("Allocations Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class AllocationsResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Integer shipmentsCount;
    private LocalDateTime cutoffDate;
    private Boolean isTemperatureControlled;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weight;
    private String weightUnit;
    @JsonSerialize(using = CustomVolumeValueSerializer.class)
    private BigDecimal volume;
    private String volumeUnit;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargable;
    private String chargeableUnit;
    private BigDecimal minTemp;
    private String minTempUnit;
    private BigDecimal maxTemp;
    private String maxTempUnit;
    @JsonSerialize(using = CustomWeightValueSerializer.class)
    private BigDecimal weightVolume;
    private String weightVolumeUnit;
    private Integer packs;
    private String packsType;
    private Integer containerCount;
    private BigDecimal teuCount;
}
