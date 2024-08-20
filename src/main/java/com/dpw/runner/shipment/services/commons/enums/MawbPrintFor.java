package com.dpw.runner.shipment.services.commons.enums;

public enum MawbPrintFor {
    PrintForAll(0,"Print For All"),
    Original3ForShipper(1,"Original 3 - (For Shipper)"),
    Original2ForConsigee(2,"Original 2 - (For Consignee)"),
    Copy9ForSalesAgent (3, "Copy 9 - (For Sales Agent)"),
    Copy4DeliveryReceipt (4, "Copy 4 - (Delivery Receipt)"),
    Original1ForIssuingCarrier (5,"Original 1 - (For Issuing Carrier)"),
    Copy5ForDestinationAirport (6, "Copy 5 - (Airport of Destination)"),
    Copy10ForCarrier (7, "Copy 10 - (Extra Copy for Carrier)"),
    Copy6ForThirdCarrier (8, "Copy 6 - (Third Carrier)"),
    Copy11ForCarrier (9, "Copy 11 - (Extra Copy for Carrier)"),
    Copy7ForCarrier (10,"Copy 7 - (For Second Carrier)"),
    Copy12ForCarrier (11,"Copy 12 - (Extra Copy for Carrier)"),
    Copy8FirstCarrier (12, "Copy 8 - (First Carrier)");

    private int id;
    private String desc;

    MawbPrintFor(int id, String desc) {
        this.id = id;
        this.desc = desc;
    }

    public String getDesc() {
        return desc;
    }

    public static MawbPrintFor getById(int id) {
        for (MawbPrintFor mawbPrintFor : MawbPrintFor.values()) {
            if(mawbPrintFor.id == id) {
                return mawbPrintFor;
            }
        }
        return null;
    }
}
