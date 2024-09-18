package com.dpw.runner.shipment.services.entity.fzb;

import lombok.*;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LinearSpatialDimensionFZB {

    @NotNull(message = "Width measure is mandatory")
    @DecimalMin(value = "0", message = "Width measure must be greater than or equal to 0")
    @DecimalMax(value = "99999", message = "Width measure must be less than or equal to 99999")
    private Double widthMeasure;

    @NotNull(message = "Length measure is mandatory")
    @DecimalMin(value = "0", message = "Length measure must be greater than or equal to 0")
    @DecimalMax(value = "99999", message = "Length measure must be less than or equal to 99999")
    private Double lengthMeasure;

    @NotNull(message = "Height measure is mandatory")
    @DecimalMin(value = "0", message = "Height measure must be greater than or equal to 0")
    @DecimalMax(value = "99999", message = "Height measure must be less than or equal to 99999")
    private Double heightMeasure;
}
