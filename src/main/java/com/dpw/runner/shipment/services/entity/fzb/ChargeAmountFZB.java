package com.dpw.runner.shipment.services.entity.fzb;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.NotNull;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ChargeAmountFZB {

    @NotNull(message = "Total charge amount currency ID is mandatory")
    private String currencyID;

    @NotNull(message = "Total charge amount value is mandatory")
    @DecimalMin(value = "0.0001", message = "Total charge amount value must be greater than or equal to 0.0001")
    @DecimalMax(value = "9999999999", message = "Total charge amount value must be less than or equal to 9999999999")
    private Double value;
}
