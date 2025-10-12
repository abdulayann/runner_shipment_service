package com.dpw.runner.shipment.services.dto.request.hbl;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@ApiModel("Hbl Revenue Charges Data Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class HblRevenueChargeDto {
    private String id;
    private String charges;
    private String currencyValue;
    private String chargeDescription;
    private String currency;
    private BigDecimal value;
    private Boolean selected;
    private String chargeCode;
}
