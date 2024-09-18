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
public class ApplicableDestinationCurrencyServiceCharge {

    @JsonProperty("CollectAppliedAmount")
    @DecimalMin(value = "0.000", message = "Applicable destination currency service charge collect applied amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable destination currency service charge collect applied amount must be less than or equal to 999999999999")
    @NotNull(message = "Applicable destination currency service charge collect applied amount cannot be null")
    private Double collectAppliedAmount;

    @JsonProperty("CollectAppliedAmount")
    private String collectAppliedAmountCurrency;

    @JsonProperty("DestinationAppliedAmount")
    @DecimalMin(value = "0.000", message = "Applicable destination currency service charge destination applied amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable destination currency service charge destination applied amount must be less than or equal to 999999999999")
    @NotNull(message = "Applicable destination currency service charge destination applied amount cannot be null")
    private Double destinationAppliedAmount;

    @JsonProperty("DestinationAppliedAmount")
    private String destinationAppliedAmountCurrency;

    @JsonProperty("TotalAppliedAmount")
    @DecimalMin(value = "0.000", message = "Applicable destination currency service charge total applied amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Applicable destination currency service charge total applied amount must be less than or equal to 999999999999")
    @NotNull(message = "Applicable destination currency service charge total applied amount cannot be null")
    private Double totalAppliedAmount;

    @JsonProperty("TotalAppliedAmount")
    private String totalAppliedAmountCurrency;

}
