package com.dpw.runner.shipment.services.syncing.Entity;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

public class Allocations {
    public BigDecimal Chargable;
    public String ChargeableUnit;
    public UUID Guid;
    public Long Id;
    public LocalDateTime CutoffDate;
    public Boolean Hazardous;
    public Boolean IsTemparatureControlled;
    public BigDecimal MaxTemp;
    public String MaxTempUnit;
    public BigDecimal MinTemp;
    public String MinTempUnit;
    public Long ShipmentsCount;
    public BigDecimal Volume;
    public String VolumeUnit;
    public String WeightUnit;
    public BigDecimal Weight;
}
