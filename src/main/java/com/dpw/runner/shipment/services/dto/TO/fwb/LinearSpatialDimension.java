package com.dpw.runner.shipment.services.dto.TO.fwb;

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
public class LinearSpatialDimension {
    @JsonProperty("WidthMeasure")
    @DecimalMin(value = "0.1", message = "Linear spatial dimension width must be greater than or equal to 0.1")
    @DecimalMax(value = "99999", message = "Linear spatial dimension width must be less than or equal to 9999")
    @NotNull(message = "Linear spatial dimension width cannot be null")
    private Double widthMeasure;

    @JsonProperty("WidthMeasureUnit")
    private String widthMeasureUnit;

    @JsonProperty("LengthMeasure")
    @DecimalMin(value = "0.1", message = "Linear spatial dimension length must be greater than or equal to 0.1")
    @DecimalMax(value = "99999", message = "Linear spatial dimension length must be less than or equal to 9999")
    @NotNull(message = "Linear spatial dimension length cannot be null")
    private Double lengthMeasure;

    @JsonProperty("LengthMeasureUnit")
    private String lengthMeasureUnit;

    @JsonProperty("HeightMeasure")
    @DecimalMin(value = "0.1", message = "Linear spatial dimension height must be greater than or equal to 0.1")
    @DecimalMax(value = "99999", message = "Linear spatial dimension height must be less than or equal to 9999")
    @NotNull(message = "Linear spatial dimension height cannot be null")
    private Double heightMeasure;

    @JsonProperty("HeightMeasureUnit")
    private String heightMeasureUnit;
}
