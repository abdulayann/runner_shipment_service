package com.dpw.runner.shipment.services.entity.fwb;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ApplicableLogisticsServiceCharge {

    @JsonProperty("TransportPaymentMethodCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid applicable logistics service charge transport payment method code provided")
    @Size(max = 2, message = "Applicable logistics service charge transport payment method code can have max length {max}")
    private String transportPaymentMethodCode;

    @JsonProperty("ServiceTypeCode")
    @Pattern(regexp = "^[a-zA-Z]*$", message = "Invalid applicable logistics service charge service type code provided")
    @Size(max = 1, message = "Applicable logistics service charge service type code can have max length {max}")
    private String serviceTypeCode;
}
