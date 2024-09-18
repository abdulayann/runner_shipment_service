package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApplicableDestinationCurrencyExchange {

    @JsonProperty("TargetCurrencyCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid applicable destination currency exchange target currency code provided")
    @Size(max = 3, message = "Applicable destination currency exchange target currency code can have max length {max}")
    @NotNull(message = "Applicable destination currency exchange target currency code cannot be null")
    private String targetCurrencyCode;

    // TODO: marketId is mentioned as Numeric(9)
    @JsonProperty("MarketId")
    @DecimalMin(value = "0.0", message = "Applicable destination currency exchange market id must be greater than or equal to 0.0")
    @DecimalMax(value = "999999999", message = "Applicable destination currency exchange market id must be greater than or equal to 999999999")
    private Double marketId;

    @JsonProperty("ConversionRate")
    @DecimalMin(value = "0.0", message = "Applicable destination currency exchange conversion rate must be greater than or equal to 0.0")
    @DecimalMax(value = "99999999999", message = "Applicable destination currency exchange conversion rate must be greater than or equal to 999999999999")
    @NotNull(message = "Applicable destination currency exchange conversion rate cannot be null")
    private Double conversionRate;
}
