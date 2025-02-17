package com.dpw.runner.shipment.services.ReportingService.Models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CommercialInvoiceModel extends ShipmentPrintModel implements IDocumentModel {
    public BigDecimal totalAmount;
    public String commercialInvoiceNumber;
    private List<String> packageSummary;
}
