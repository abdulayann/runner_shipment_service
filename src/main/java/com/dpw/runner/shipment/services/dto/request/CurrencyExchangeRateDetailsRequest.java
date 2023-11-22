package com.dpw.runner.shipment.services.dto.request;

import lombok.Getter;
import lombok.Setter;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

@Getter
@Setter
public class CurrencyExchangeRateDetailsRequest implements Serializable {
    @NotNull(message = "Exchange rate type can not be null")
    private ExchangeRateType type;//VENDOR/CUSTOMER/REPORT/INVOICE
    @NotBlank(message = "Exchange rate type can not be null")
    private String currency;
    @NotNull(message = "currency quantity type can not be null")
    private BigDecimal currencyQuantity;
    @NotBlank(message = "Base currency type can not be null")
    private String baseCurrency;
    @NotNull(message = "Base currency quantity type can not be null")
    private BigDecimal baseCurrencyQuantity;
    private String source;//RUNNER for now
    private Boolean isReciprocalCurrency;
    @NotNull(message = "Exchange rate can not be null")
    private BigDecimal exchangeRate;
}