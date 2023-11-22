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
public class SubTaxDetailsRequest implements Serializable {
    @NotNull(message = "Sub Tax percentage can not be null")

    private BigDecimal percentage;
    private String taxCode;
    @NotNull(message = "Sub Tax amount can not be null")
    private BigDecimal amount;
    private String action;
    private String description;
}