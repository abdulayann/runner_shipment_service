package com.dpw.runner.shipment.services.dto.request;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

@Getter
@Setter
@EqualsAndHashCode
@ToString
public class TaxDetailsRequest implements Serializable {
    @NotNull(message = "Tax type can not be null")
    private TaxType taxType;

    @NotNull(message = "Tax percentage can not be null")
    private BigDecimal percentage;
    @NotNull(message = "Tax amount can not be null")
    private BigDecimal amount;
}
