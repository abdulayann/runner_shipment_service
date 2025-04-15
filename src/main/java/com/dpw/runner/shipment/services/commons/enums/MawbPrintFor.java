package com.dpw.runner.shipment.services.commons.enums;

public enum MawbPrintFor {
    PrintForAll(0,"Print For All", false),
    Original3ForShipper(1,"Original 3 - (For Shipper)", true),
    Original2ForConsigee(2,"Original 2 - (For Consignee)", true),
    Copy9ForSalesAgent (3, "Copy 9 - (For Sales Agent)", false),
    Copy4DeliveryReceipt (4, "Copy 4 - (Delivery Receipt)", false),
    Original1ForIssuingCarrier (5,"Original 1 - (For Issuing Carrier)", true),
    Copy5ForDestinationAirport (6, "Copy 5 - (Airport of Destination)", false),
    Copy10ForCarrier (7, "Copy 10 - (Extra Copy for Carrier)", false),
    Copy6ForThirdCarrier (8, "Copy 6 - (Third Carrier)", false),
    Copy11ForCarrier (9, "Copy 11 - (Extra Copy for Carrier)", false),
    Copy7ForCarrier (10,"Copy 7 - (For Second Carrier)", false),
    Copy12ForCarrier (11,"Copy 12 - (Extra Copy for Carrier)", false),
    Copy8FirstCarrier (12, "Copy 8 - (First Carrier)", false);

    private int id;
    private String desc;
    private final boolean printTermsAndCondition;

    MawbPrintFor(int id, String desc, boolean printTermsAndCondition) {
        this.id = id;
        this.desc = desc;
        this.printTermsAndCondition = printTermsAndCondition;
    }

    public String getDesc() {
        return desc;
    }

    public boolean getPrintTermsAndCondition() {
        return printTermsAndCondition;
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
