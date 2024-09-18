package com.dpw.runner.shipment.services.entity.fzb;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor

public class AssociatedUnitLoadTransportEquipmentFZB {

    @NotNull(message = "Associated unit load transport equipment ID is mandatory")
    @Pattern(regexp = "\\d{1,5}", message = "Associated unit load transport equipment ID must be a numeric value with 1 to 5 digits")
    private String id;

    private Double tareWeightMeasure;

    @Min(value = 1, message = "Loaded package quantity must be at least 1")
    private Integer loadedPackageQuantity;

    @NotNull(message = "Characteristic code is mandatory")
    @Size(max = 3, message = "Characteristic code must not exceed 3 characters")
    private String characteristicCode;

    @NotNull(message = "Primary ID is mandatory")
    private String operatingPartyId;
}
