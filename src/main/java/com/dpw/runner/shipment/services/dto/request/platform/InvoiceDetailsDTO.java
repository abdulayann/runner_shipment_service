package com.dpw.runner.shipment.services.dto.request.platform;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@NoArgsConstructor
@AllArgsConstructor
@Data
@SuppressWarnings("java:S1948")
public class InvoiceDetailsDTO implements Serializable {
    private String invoiceId;
    private String invoiceNumber;
    private String documentType;
    private String clientId;
    private String clientAddressId;
    private String jobStatus;
    private String invoiceDate;
    private String dueDate;
    private String amount;
    private String taxAmount;
    private String totalInvoiceAmount;
    private String invoiceCurrency;
    private TotalChargesDTO total_charges;
}
