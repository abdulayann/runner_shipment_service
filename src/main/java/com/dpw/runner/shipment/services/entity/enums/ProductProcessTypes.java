package com.dpw.runner.shipment.services.entity.enums;

public enum ProductProcessTypes {

    INVOICE(1, "Invoice"),
    CREDIT_NOTE(2, "Credit Note"),
    DEBIT_NOTE(3, "Debit Note"),
    ALL(4, "All"),
    QUOTE_NUMBER(5, "QuoteNumber"),
    HBL_NUMBER(6, "HBLNumber"),
    SHIPMENT_NUMBER(7, "ShipmentNumber"),
    REFERENCE_NUMBER(8, "ReferenceNumber"),
    BOL_NUMBER(9, "BOLNumber"),
    AR_INVOICE_NUMBER(10, "ARInvoiceNumber"),
    AP_INVOICE_NUMBER(11, "APInvoiceNumber"),
    WAY_BILL_NUMBER(13, "WayBillNumber"),
    HAWB(14, "HAWB"),
    SEA(15, "SEA"),
    AIR(16, "AIR"),
    PAYMENT(17, "Payment"),
    TAX(18, "Tax"),
    CSR(20, "Cargo Sales Report"),
    RECEIPT(21, "Receipt"),
    CONSOL_SHIPMENT_TI(22, "Consol_Shipment_TI");
    private final int value;
    private final String description;

    ProductProcessTypes(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getValue() {
        return value;
    }
}
