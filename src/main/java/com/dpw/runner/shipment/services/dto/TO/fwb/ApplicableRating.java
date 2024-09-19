package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.dpw.runner.shipment.services.entity.enums.RatingTypeIndicator;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.Valid;
import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ApplicableRating {


    /**  F for Face, P for Published, or A for Actual Rating detail. */
    @JsonProperty("TypeCode")
    private RatingTypeIndicator typeCode = RatingTypeIndicator.F;

    @JsonProperty("TotalChargeAmount")
    @DecimalMin(value = "0.0", message = "Applicable rating total charge Amount must be greater than or equal to 0.0")
    @DecimalMax(value = "999999999999", message = "Applicable rating total charge Amount must be greater than or equal to 999999999999")
    private Double totalChargeAmount;

    // TODO: if totalChargeAmount has value then totalChargeCurrency cannot be null
    @JsonProperty("TotalChargeCurrency")
    private String totalChargeCurrency;

    @JsonProperty("ConsignmentItemQuantity")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Applicable rating consignment item quantity provided")
    @Size(max = 5, message = "Applicable rating consignment item quantity can have max length {max}")
    private String consignmentItemQuantity;

    @Valid
    @JsonProperty("IncludedMasterConsignmentItem")
    @Size(min = 1, message = "At least one included master consignment should be provided")
    private List<IncludedMasterConsignmentItem> includedMasterConsignmentItems;

}
