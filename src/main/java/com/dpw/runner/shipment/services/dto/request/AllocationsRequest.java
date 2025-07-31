package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
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
}