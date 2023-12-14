package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class AwbGoodsDescriptionInfoV2 {
    // public Int64 entityId;
    public String entityType;
    public Integer piecesNo;
    public BigDecimal grossWt;
    public String grossWtUnit;
    public Integer rateClass;
    public Integer commodityItemNo;
    public BigDecimal chargeableWt;
    public BigDecimal rateCharge;
    public BigDecimal totalAmount;
    public Integer slaCCode;
    public String hsCode;
}
