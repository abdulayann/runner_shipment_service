package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class AwbPaymentInfoV2 implements IRunnerRequest {
    public String entityType;
    public BigDecimal weightCharges;
    public BigDecimal valuationCharge;
    public BigDecimal tax;
    public BigDecimal dueAgentCharges;
    public BigDecimal dueCarrierCharges;
    public BigDecimal totalPrepaid;
    public BigDecimal totalCollect;
}
