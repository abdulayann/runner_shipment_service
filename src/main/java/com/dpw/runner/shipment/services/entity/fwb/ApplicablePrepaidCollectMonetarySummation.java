package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApplicablePrepaidCollectMonetarySummation {
    @JsonProperty("PrepaidIndicator")
    @NotNull(message = "Applicable prepaid collect monetary summation prepaid indicator cannot be null")
    private Boolean prepaidIndicator;

    @JsonProperty("WeightChargeTotalAmount")
    @DecimalMin(value = "0.000", message = "Applicable prepaid collect monetary summation weight charge total amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable prepaid collect monetary summation weight charge total amount must be less than or equal to 999999999999")
    private Double weightChargeTotalAmount;

    @JsonProperty("WeightChargeTotalAmountCurrency")
    private String weightChargeTotalAmountCurrency;

    @JsonProperty("ValuationChargeTotalAmount")
    @DecimalMin(value = "0.000", message = "Applicable prepaid collect monetary summation valuation charge total amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable prepaid collect monetary summation valuation charge total amount must be less than or equal to 999999999999")
    private Double valuationChargeTotalAmount;

    @JsonProperty("ValuationChargeTotalAmountCurrency")
    private String valuationChargeTotalAmountCurrency;

    @JsonProperty("TaxTotalAmount")
    @DecimalMin(value = "0.000", message = "Applicable prepaid collect monetary summation tax total amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable prepaid collect monetary summation tax total amount must be less than or equal to 999999999999")
    private Double taxTotalAmount;

    @JsonProperty("TaxTotalAmountCurrency")
    private String taxTotalAmountCurrency;

    @JsonProperty("AgentTotalDuePayableAmount")
    @DecimalMin(value = "0.000", message = "Applicable prepaid collect monetary summation agent total due payable amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable prepaid collect monetary summation agent total due payable amount must be less than or equal to 999999999999")
    private Double agentTotalDuePayableAmount;

    @JsonProperty("AgentTotalDuePayableAmountCurrency")
    private String agentTotalDuePayableAmountCurrency;

    @JsonProperty("CarrierTotalDuePayableAmount")
    @DecimalMin(value = "0.000", message = "Applicable prepaid collect monetary summation carrier total due payable amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable prepaid collect monetary summation carrier total due payable amount must be less than or equal to 999999999999")
    private Double carrierTotalDuePayableAmount;

    @JsonProperty("CarrierTotalDuePayableAmountCurr")
    private String carrierTotalDuePayableAmountCurrency;

    @JsonProperty("GrandTotalAmount")
    @DecimalMin(value = "0.000", message = "Applicable prepaid collect monetary summation grand total amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable prepaid collect monetary summation grand total amount must be less than or equal to 999999999999")
    @NotNull(message = "Applicable prepaid collect monetary summation grand total amount cannot be null")
    private Double grandTotalAmount;

    @JsonProperty("GrandTotalAmountCurrency")
    private String grandTotalAmountCurrency;
}
