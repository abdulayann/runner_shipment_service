package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.fwb.LinearSpatialDimension;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TransportLogisticsPackage {

    @JsonProperty("ItemQuantity")
    @NotNull(message = "Transport logistics package item quantity cannot be null")
    @DecimalMin(value = "0.1", message = "Transport logistics package item quantity must be greater than or equal to 0.1")
    @DecimalMax(value = "9999", message = "Transport logistics package item quantity must be less than or equal to 9999")
    private Double itemQuantity;

    @JsonProperty("GrossWeightMeasure")
    @DecimalMin(value = "0.1", message = "Transport logistics package gross weight measure must be greater than or equal to 0.1")
    @DecimalMax(value = "9999999", message = "Transport logistics package gross weight measure must be less than or equal to 9999999")
    private Double grossWeightMeasure;

    @JsonProperty("GrossWeightMeasureUnit")
    private String grossWeightMeasureUnit;

    @Valid
    @JsonProperty("LinearSpatialDimension")
    private LinearSpatialDimension linearSpatialDimension;

}
