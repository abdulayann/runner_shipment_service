package com.dpw.runner.shipment.services.dto.TO.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.*;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApplicableLogisticsAllowanceCharge {


    // TODO The combination of "Other Charge Code" and "Charge Entitlement Code Party" cannot be duplicated
    //  (another combination of the same Other Charge Code and the same Entitlement Code is not allowed)

    @JsonProperty("ID")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Applicable logistics allowance charge id provided")
    @Size(max = 2, message = "Applicable logistics allowance charge id can have max length {max}")
    @NotNull(message = "Applicable logistics allowance charge id cannot be null")
    private String id;

    @JsonProperty("AdditionalId")
    private LocalDateTime additionalId;

    @JsonProperty("PrepaidIndicator")
    @NotNull(message = "Applicable logistics allowance charge prepaid indicator cannot be null")
    private Boolean prepaidIndicator;

    @JsonProperty("LocationTypeCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid Applicable logistics allowance charge location type code provided")
    @Size(max = 1, message = "Applicable logistics allowance charge location type code can have max length {max}")
    private String locationTypeCode;

    @JsonProperty("Reason")
    @Pattern(regexp = "^[a-zA-Z0-9]*$", message = "Invalid Applicable logistics allowance charge reason provided")
    @Size(max = 70, message = "Applicable logistics allowance charge reason can have max length {max}")
    private String reason;

    //todo: The combination of "Other Charge Code" and "Charge Entitlement Code Party" cannot be duplicated
    // (another combination of the same Other Charge Code and the same Entitlement Code is not allowed)

    @NotNull(message = "Applicable logistics allowance charge party type code cannot be null")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid applicable logistics allowance charge party type code provided")
    @Size(max = 1, message = "Applicable logistics allowance charge party type code can have max length {max}")
    @JsonProperty("PartyTypeCode")
    private String partyTypeCode;

    @JsonProperty("ActualAmount")
    @NotNull(message = "Applicable logistics allowance charge actual amount cannot be null")
    @DecimalMin(value = "0.0", message = "Applicable logistics allowance charge actual amount must be greater than or equal to 0.0")
    @DecimalMax(value = "999999999999", message = "Applicable logistics allowance charge actual amount must be greater than or equal to 999999999999")
    private Double actualAmount;

    // todo: cannot be null if actual amount has value
    @JsonProperty("ActualAmountCurrency")
    @NotNull(message = "OTH curr can not be null")
    private String actualAmountCurrency;

    @JsonProperty("TimeBasisQuantity")
    @Size(max = 35, message = "Applicable logistics allowance charge time basis quantity can have max length {max}")
    private String timeBasisQuantity;

    @JsonProperty("ItemBasisQuantity")
    @DecimalMin(value = "0.0", message = "Applicable logistics allowance charge item basis quantity must be greater than or equal to 0.0")
    @DecimalMax(value = "99999", message = "Applicable logistics allowance charge item basis quantity must be greater than or equal to 99999")
    private Double itemBasisQuantity;

    @JsonProperty("ServiceDate")
    private LocalDateTime serviceDate;

    @JsonProperty("SpecialServiceDescription")
    @Size(max = 35, message = "Applicable logistics allowance charge special service description can have max length {max}")
    private String specialServiceDescription;

    @JsonProperty("SpecialServiceTime")
    private LocalDateTime specialServiceTime;


}
