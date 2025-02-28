package com.dpw.runner.shipment.services.reportingservice.Models;

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
public class CommercialInvoiceModel extends ShipmentPrintModel implements IDocumentModel{
    private List<String> packageSummary;
    public BigDecimal totalAmount;
    public String commercialInvoiceNumber;
}
