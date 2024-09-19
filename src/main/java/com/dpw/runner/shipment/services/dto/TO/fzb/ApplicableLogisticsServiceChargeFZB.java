package com.dpw.runner.shipment.services.dto.TO.fzb;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class ApplicableLogisticsServiceChargeFZB {
    @NotNull(message = "Transport payment method code is mandatory")
    @Pattern(regexp = "[A-Za-z]{2}", message = "Transport payment method code must contain only uppercase and lowercase letters with a maximum length of 2 characters")
    private String transportPaymentMethodCode;

    @NotNull(message = "Service type code is mandatory")
    @Pattern(regexp = "[A-Za-z]", message = "Service type code must contain only uppercase and lowercase letters with a maximum length of 1 character")
    private String serviceTypeCode;
}
