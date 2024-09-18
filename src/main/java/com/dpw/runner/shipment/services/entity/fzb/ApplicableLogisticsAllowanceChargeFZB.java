package com.dpw.runner.shipment.services.entity.fzb;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ApplicableLogisticsAllowanceChargeFZB {

    @NotNull(message = "Other charge code is mandatory")
    @Pattern(regexp = "[A-Za-z]{2}", message = "Other charge code must contain only uppercase and lowercase letters with a maximum length of 2 characters")
    private String id;

    @Pattern(regexp = "[A-Za-z0-9 ]{0,70}", message = "Reason must contain alphanumeric characters and spaces with a maximum length of 70 characters")
    private String reason;

    @NotNull(message = "Party type code is mandatory")
    @Pattern(regexp = "[A-Za-z]", message = "Party type code must contain only uppercase and lowercase letters with a length of 1 character")
    private String partyTypeCode;

    @NotNull(message = "Actual amount is mandatory")
    @DecimalMin(value = "0.000", message = "Actual amount must be greater than or equal to 0.000")
    @DecimalMax(value = "999999999999", message = "Actual amount must be less than or equal to 999999999999")
    private Double actualAmount;
}
