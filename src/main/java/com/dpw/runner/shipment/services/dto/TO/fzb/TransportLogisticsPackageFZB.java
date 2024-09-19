package com.dpw.runner.shipment.services.dto.TO.fzb;

import lombok.*;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TransportLogisticsPackageFZB {

    @NotNull(message = "Item quantity is mandatory")
    @Min(value = 1, message = "Item quantity must be at least 1")
    private Integer itemQuantity;

    @NotNull(message = "Gross weight measure is mandatory")
    private WeightMeasureFZB grossWeightMeasure;

    private LinearSpatialDimensionFZB linearSpatialDimension;
}
