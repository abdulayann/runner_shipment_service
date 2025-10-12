package com.dpw.runner.shipment.services.dto.request.billing;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class RevenueChargeDto {
    private String id;
    private BigDecimal localAmount;
    private BigDecimal overseasAmount;
    private BigDecimal totalOverseasAmount;
    private String overseasCurrency;
    private String localCurrency;
    private String details;
    private String chargeTypeCode;
    private String chargeTypeDescription;
    private String moduleNumber;
    private String moduleTypeCode;
    private String moduleGuid;
    private String houseBill;
    private String status;
}