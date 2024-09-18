package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApplicableUnitLoadDeviceRateClass {

    @JsonProperty("TypeCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid applicable unit load device rate class type code provided")
    @Size(max = 3, message = "Applicable unit load device rate class type code can have max length {max}")
    private String typeCode;

    @JsonProperty("BasisCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid applicable unit load device rate class basis code provided")
    @Size(max = 1, message = "Applicable unit load device rate class basis code can have max length {max}")
    private String basisCode;

    @JsonProperty("AppliedPercent")
    @Pattern(regexp = "^(\\d{3})?$", message = "Invalid applicable unit load device rate class applied percent provided")
    @DecimalMin(value = "0.0", message = "Applicable unit load device rate class applied percent must be greater than or equal to 0.0")
    @DecimalMax(value = "999", message = "Applicable unit load device rate class applied percent must be greater than or equal to 999")
    private Double appliedPercent;

    @JsonProperty("ReferenceID")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid applicable unit load device rate class reference id provided")
    @Size(max = 35, message = "Applicable unit load device rate class reference id can have max length {max}")
    private String referenceID;

    @JsonProperty("ReferenceTypeCode")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid applicable unit load device rate class reference type code provided")
    @Size(max = 35, message = "Applicable unit load device rate class reference type code can have max length {max}")
    private String referenceTypeCode;

}
