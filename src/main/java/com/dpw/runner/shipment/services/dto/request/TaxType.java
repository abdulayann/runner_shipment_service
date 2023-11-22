package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum TaxType {
    IGST("IGST"), CGST("CGST"), UGST("UGST"), SGST("SGST"), VAT("VAT");
    private final String code;
}
