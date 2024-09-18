package com.dpw.runner.shipment.services.entity.fzb;


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
public class WeightMeasureFZB {

    @JsonProperty("Code")
    @NotNull(message = "Gross measure unit code is mandatory")
    private String unitCode;

    @JsonProperty("Value")
    @NotNull(message = "Gross value is mandatory")
    @DecimalMin(value = "0.1", message = "Gross weight measure value must be greater than or equal to 0.1")
    @DecimalMax(value = "9999999", message = "Gross weight measure value must be less than or equal to 9999999")
    private Double value;
}
