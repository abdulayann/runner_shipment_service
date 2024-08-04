package com.dpw.runner.shipment.services.masterdata.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import lombok.Data;

@Data
public class BillChargesResponse {
    @JsonProperty("ChargeTypeId")
    private Long chargeTypeId;
    @JsonProperty("OverseasSellAmount")
    private BigDecimal overseasSellAmount;
    @JsonProperty("OverseasSellCurrency")
    private String overseasSellCurrency;
    @JsonProperty("LocalSellAmount")
    private BigDecimal localSellAmount;
    @JsonProperty("LocalSellCurrency")
    private String localSellCurrency;
    @JsonProperty("OverseasTax")
    private BigDecimal overseasTax; //TODO: SUBHAM doubt if its for cost / revenue
    @JsonProperty("SellExchange")
    private BigDecimal sellExchange; //TODO: SUBHAM doubt which value is this
    @JsonProperty("TaxType1")
    private BigDecimal taxType1; //TODO: SUBHAM doubt which value is this
    @JsonProperty("TaxType2")
    private BigDecimal taxType2; //TODO: SUBHAM doubt which value is this
    @JsonProperty("TaxType3")
    private BigDecimal taxType3; //TODO: SUBHAM doubt which value is this
    @JsonProperty("TaxType4")
    private BigDecimal taxType4; //TODO: SUBHAM doubt which value is this
    @JsonProperty("PaymentType")
    private String paymentType;
    @JsonProperty("ChargeTypeCode")
    private String chargeTypeCode;
    @JsonProperty("ChargeTypeDescription")
    private String chargeTypeDescription;
    @JsonProperty("LocalTax")
    private BigDecimal localTax; //TODO: SUBHAM doubt which value is this
    @JsonProperty("MeasurementBasis")
    private String measurementBasis; //TODO: SUBHAM doubt which value is this
    @JsonProperty("LocalCostCurrency")
    private String localCostCurrency;

    // Billing Service attributes
    private String billingChargeTypeId;
    private String billingChargeTypeGuid;

}
