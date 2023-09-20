package com.dpw.runner.shipment.services.ReportingService.Models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
public class CommercialInvoiceModel extends ShipmentPrintModel implements IDocumentModel{
    public List<String> packageSummary;
    public BigDecimal totalAmount;
    public String commercialInvoiceNumber;
}
