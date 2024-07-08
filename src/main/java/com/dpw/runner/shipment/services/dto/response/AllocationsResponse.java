package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.config.CustomWeightVolumeJsonSerializer;
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
    private Boolean hazardous;
    private LocalDateTime cutoffDate;
    private Boolean isTemperatureControlled;
    @JsonSerialize(using = CustomWeightVolumeJsonSerializer.CustomWeightValueJsonSerializer.class)
    private BigDecimal weight;
    private String weightUnit;
    @JsonSerialize(using = CustomWeightVolumeJsonSerializer.CustomVolumeValueJsonSerializer.class)
    private BigDecimal volume;
    private String volumeUnit;
    @JsonSerialize(using = DecimalPlaceValueSerializer.class)
    private BigDecimal chargable;
    private String chargeableUnit;
    private BigDecimal minTemp;
    private String minTempUnit;
    private BigDecimal maxTemp;
    private String maxTempUnit;
}
