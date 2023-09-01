package com.dpw.runner.shipment.services.entity.enums;

public enum ProductProcessTypes {

    Invoice(1, "Invoice"),
    Credit_Note(2, "Credit Note"),
    Debit_Note(3, "Debit Note"),
    All(4, "All"),
    QuoteNumber(5, "QuoteNumber"),
    HBLNumber(6, "HBLNumber"),
    ShipmentNumber(7, "ShipmentNumber"),
    ReferenceNumber(8, "ReferenceNumber"),
    BOLNumber(9, "BOLNumber"),
    ARInvoiceNumber(10, "ARInvoiceNumber"),
    APInvoiceNumber(11, "APInvoiceNumber"),
    WayBillNumber(13, "WayBillNumber"),
    HAWB(14, "HAWB"),
    SEA(15, "SEA"),
    AIR(16, "AIR"),
    PAYMENT(17, "Payment"),
    TAX(18, "Tax"),
    CSR(20, "Cargo Sales Report"),
    Receipt(21, "Receipt");
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
