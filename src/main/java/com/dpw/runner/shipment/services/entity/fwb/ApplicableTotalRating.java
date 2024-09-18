package com.dpw.runner.shipment.services.entity.fwb;

import com.dpw.runner.shipment.services.entity.enums.RatingTypeIndicator;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApplicableTotalRating {

    @JsonProperty("TypeCode")
    @NotNull(message = "Applicable total rating type code cannot be null")
    private RatingTypeIndicator typeCode = RatingTypeIndicator.F;

    @Valid
    @JsonProperty("ApplicableDestinationCurrencyServiceCharge")
    private ApplicableDestinationCurrencyServiceCharge applicableDestinationCurrencyServiceCharge;

    @Valid
    @JsonProperty("ApplicablePrepaidCollectMonetarySummation")
    @Size(min = 1, message = "At least one applicable prepaid collect monetary summation is required")
    private List<ApplicablePrepaidCollectMonetarySummation> applicablePrepaidCollectMonetarySummation;

}
