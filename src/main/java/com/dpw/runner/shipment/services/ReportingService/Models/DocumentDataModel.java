package com.dpw.runner.shipment.services.ReportingService.Models;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class DocumentDataModel implements IDocumentModel {
    private BigDecimal totalCGSTAmount;
    private BigDecimal totalSGSTAmount;
    private BigDecimal totalIGSTAmount;
    private BigDecimal totalTaxableAmount;
    private BigDecimal totalNonTaxableAmount;
    private BigDecimal totalUGSTAmount;
    private BigDecimal totalSubTaxAmount;

    public DocumentDataModel() {
        totalCGSTAmount = BigDecimal.ZERO;
        totalSGSTAmount = BigDecimal.ZERO;
        totalIGSTAmount = BigDecimal.ZERO;
        totalTaxableAmount = BigDecimal.ZERO;
        totalNonTaxableAmount = BigDecimal.ZERO;
        totalUGSTAmount = BigDecimal.ZERO;
        totalSubTaxAmount = BigDecimal.ZERO;
    }
}
