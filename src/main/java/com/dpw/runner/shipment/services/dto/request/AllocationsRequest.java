package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;
import org.apache.poi.hpsf.Decimal;

import javax.persistence.Column;
import java.time.LocalDateTime;
import java.util.UUID;


@Data
@Builder
@ApiModel("Allocations Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class AllocationsRequest implements IRunnerRequest {
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