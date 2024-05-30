package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AwbGoodsDescriptionInfoV2 implements IRunnerRequest {
    // public Int64 entityId;
    public UUID guid;
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
