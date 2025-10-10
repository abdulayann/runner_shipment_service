package com.dpw.runner.shipment.services.dto.request.hbl;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class HblRevenueChargeDto {
    // private Integer id;
    private UUID guid;
    private String charges;
    private String chargeDescription;
    private String currency;
    private BigDecimal value;
    private Boolean selected;
    private String chargeCode;
    private Boolean isPrintRequired; // For future print control
}
