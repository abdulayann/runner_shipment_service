package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class AwbOtherChargesInfoV2 implements IRunnerRequest {
    // public Integer entityId;
    public String entityType;
    public String chargeTypeId;
    public BigDecimal rate;
    public Integer chargeBasis;
    public BigDecimal amount;
    public String modeOfPayment;
    public Integer chargeDue;
    public String iataDescription;
    public String chargeTypeDescription;
    public BigDecimal awbChargeCodeDefaultVat;
    public Long v2ChargeId;
}
