package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum ExchangeRateType {
    VENDOR("VENDOR"),
    CUSTOMER("CUSTOMER"),
    REPORT("REPORT"),
    REVENUE_VENDOR("REVENUE_VENDOR");
    private final String value;
}

