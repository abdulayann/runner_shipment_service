package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class AwbCargoInfoV2 implements IRunnerRequest {
    public String entityType;
    public String accountingInfo;
    public String handlingInfo;
    public String otherInfo;
    public String ntrQtyGoods;
    public String referenceNumber;
    public String shippingInformation;
    public String shippingInformationOther;
    public String sci;
    public String currency;
    public String chargeCode;
    public BigDecimal carriageValue;
    public BigDecimal customsValue;
    public BigDecimal insuranceAmount;
    public String customOriginCode;
}
