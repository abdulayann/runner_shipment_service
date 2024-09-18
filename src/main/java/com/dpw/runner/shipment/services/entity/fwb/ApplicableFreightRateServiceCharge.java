package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApplicableFreightRateServiceCharge {

    @JsonProperty("CategoryCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Applicable freight rate service charge category code provided")
    @Size(max = 1, message = "Applicable freight rate service charge category code can have max length {max}")
    private String categoryCode;

    @JsonProperty("CommodityItemID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Applicable freight rate service charge commodity item id provided")
    @Size(max = 11, message = "Applicable freight rate service charge commodity item id can have max length {max}")
    private String commodityItemID;

    @JsonProperty("ChargeableWeightMeasure")
    @NotNull(message = "Applicable freight rate service charge chargeable weight measure cannot be null")
    @DecimalMin(value = "0.1", message = "Applicable freight rate service charge chargeable weight measure must be greater than or equal to 0.1")
    @DecimalMax(value = "9999999", message = "Applicable freight rate service charge chargeable weight measure must be less than or equal to 9999999")
    private Double chargeableWeightMeasure;

    @JsonProperty("ChargeableWeightMeasureUnit")
    @NotNull(message = "Applicable freight rate service charge chargeable weight measure unit cannot be null")
    private String chargeableWeightMeasureUnit;

    @JsonProperty("AppliedRate")
    @NotNull(message = "Applicable freight rate service applied rate cannot be null")
    @DecimalMin(value = "0.0001", message = "Applicable freight rate service charge applied rate must be greater than or equal to 0.0001")
    @DecimalMax(value = "99999999", message = "Applicable freight rate service charge applied rate must be less than or equal to 99999999")
    private Double appliedRate;

    @JsonProperty("AppliedAmount")
    @NotNull(message = "Applicable freight rate service applied amount cannot be null")
    @DecimalMin(value = "0.00000000001", message = "Applicable freight rate service charge applied amount must be greater than or equal to 0.00000000001")
    @DecimalMax(value = "999999999999", message = "Applicable freight rate service charge applied amount must be less than or equal to 999999999999")
    private Double appliedAmount;

    @JsonProperty("AppliedAmountCurrency")
    @NotNull(message = "Applicable freight rate service applied amount currency cannot be null")
    private String appliedAmountCurrency;
}
