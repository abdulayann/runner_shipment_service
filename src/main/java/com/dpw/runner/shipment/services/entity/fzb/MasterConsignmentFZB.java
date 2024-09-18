package com.dpw.runner.shipment.services.entity.fzb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class MasterConsignmentFZB {

//    @Valid
//    private WeightMeasureFZB includedTareGrossWeightMeasure;

    @JsonProperty("IncludedTareGrossWeightMeasure")
    @NotNull(message = "Total gross weight is mandatory")
    //@Pattern(regexp = "\\p{Alnum}{1,7}", message = "Total gross weight must be an alphanumeric string with at most 7 characters")
    private String includedTareGrossWeightMeasure;

    @NotNull(message = "Total gross weight Unit is mandatory")
    @JsonProperty("IncludedTareGrossWeightMeasureUnitCode")
    private String includedTareGrossWeightMeasureUnit;

    //@NotNull(message = "Master Waybill Total number of pieces is mandatory")
    @JsonProperty("TotalPieceQuantity")
    @Max(value = 9999, message = "Waybill Total number of pieces must be less than or equal to 9999")
    private Integer totalPieceQuantity;

    @JsonProperty("MasterAwbNumber")
    @NotNull(message = "Master Way Bill Number is mandatory")
    @Size(min = 12, max = 12, message = "Master Way Bill Number must be 12 characters long")
    @Pattern(regexp = "\\d{3}-\\d{8}", message = "Invalid Master Way Bill format")
    private String transportContractDocumentId;

    @JsonProperty("OriginLocation")
    @Valid
    private LocationFZB originLocation;

    @JsonProperty("FinalDestinationLocation")
    @Valid
    private LocationFZB finalDestinationLocation;

    @JsonProperty("IncludedHouseConsignment")
    @Valid
    private IncludedHouseConsignmentFZB includedHouseConsignment;
}
