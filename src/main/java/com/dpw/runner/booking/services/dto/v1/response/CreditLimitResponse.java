package com.dpw.runner.booking.services.dto.v1.response;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CreditLimitResponse {
    @JsonProperty("CreditLimit")
    public BigDecimal creditLimit;
    @JsonProperty("AvailableCreditLimit")
    public BigDecimal availableCreditLimit;
}
