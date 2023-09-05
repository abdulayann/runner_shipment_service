package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
public class Allocations {
    private BigDecimal Chargable;
    private String ChargeableUnit;
    private UUID Guid;
    private Long Id;
    private LocalDateTime CutoffDate;
    private Boolean Hazardous;
    private Boolean IsTemparatureControlled;
    private BigDecimal MaxTemp;
    private String MaxTempUnit;
    private BigDecimal MinTemp;
    private String MinTempUnit;
    private Long ShipmentsCount;
    private BigDecimal Volume;
    private String VolumeUnit;
    private String WeightUnit;
    private BigDecimal Weight;
}
