package com.dpw.runner.shipment.services.ReportingService.Models.Commons;

import lombok.Data;

@Data
public class TaxPair<T, U> {
    private T taxType;
    private U taxValue;

    public TaxPair(T taxType, U taxValue) {
        this.taxType = taxType;
        this.taxValue = taxValue;
    }
}
