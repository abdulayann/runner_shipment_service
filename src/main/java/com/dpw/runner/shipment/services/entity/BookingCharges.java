package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.dto.request.PartiesRequest;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public class BookingCharges {
    public Long bookingId;
    public String seqNo;
    public String chargeType; //charge type master data
    public String details;
    public String chargeCodeAlt;
    public String hsnMaster; //HSN SAC Master data
    public String measurementBasis; //enum
    public String measurementContainerType; //container type master data
    public BigDecimal totalUnitCount;
    public String measurementUnit;
    public String costCurrencyExchangeUpdate; //enum
    public Boolean reciprocalCurrencyCost;
    public BigDecimal estimatedCost;
    public BigDecimal localCostAmount;
    public BigDecimal overseasCostAmount;
    public String overseasCostCurrency; //currencies master data
    public BigDecimal localCostCurrency; //currencies master data
    public BigDecimal currentCostRate;
    public String costRateCurrency; //currencies master data
    public BigDecimal costExchange;
    public String costAccount;
    public String costComments;
    public PartiesRequest creditor;
    public String costTaxId;
    public LocalDateTime costTaxDate;
    public BigDecimal costLocalTax;
    public BigDecimal costTaxPercentage;
    public BigDecimal costOverseasTax;
    public Boolean costNoGST;
    public BigDecimal costTaxType1;
    public BigDecimal costTaxType2;
    public BigDecimal costTaxType3;
    public BigDecimal costTaxType4;
    public BigDecimal costLineTotal;
    public String sellCurrencyExchangeUpdate; //enum
    public Boolean reciprocalCurrencyRevenue;
    public BigDecimal estimatedRevenue;
    public BigDecimal localSellAmount;
    public BigDecimal overseasSellAmount;
    public String overseasSellCurrency; //currencies master data
    public BigDecimal localSellCurrency; //currencies master data
    public BigDecimal currentSellRate;
    public String sellRateCurrency; //currencies master data
    public BigDecimal sellExchange;
    public String revenueAccount;
    public String revenueComments;
    public PartiesRequest debtor;
    public String revenueTaxId;
    public LocalDateTime revenueTaxDate;
    public BigDecimal localTax;
    public BigDecimal taxPercentage;
    public BigDecimal overseasTax;
    public Boolean noGST;
    public BigDecimal taxType1;
    public BigDecimal taxType2;
    public BigDecimal taxType3;
    public BigDecimal taxType4;
    public BigDecimal revenueLineTotal;
}
