package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;
import org.apache.poi.hpsf.Decimal;

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
    private Decimal weight;
    private String weightUnit;
    private Decimal volume;
    private String volumeUnit;
    private Decimal Chargable;
    private String ChargeableUnit;
    private Decimal minTemp;
    private String minTempUnit;
    private Decimal maxTemp;
    private String maxTempUnit;
}
