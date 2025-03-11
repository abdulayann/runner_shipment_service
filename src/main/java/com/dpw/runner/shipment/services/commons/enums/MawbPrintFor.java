package com.dpw.runner.shipment.services.commons.enums;

public enum MawbPrintFor {
    PRINT_FOR_ALL(0,"Print For All", false),
    ORIGINAL_3_FOR_SHIPPER(1,"Original 3 - (For Shipper)", true),
    ORIGINAL_2_FOR_CONSIGEE(2,"Original 2 - (For Consignee)", true),
    COPY_9_FOR_SALES_AGENT(3, "Copy 9 - (For Sales Agent)", false),
    COPY_4_DELIVERY_RECEIPT(4, "Copy 4 - (Delivery Receipt)", false),
    ORIGINAL_1_FOR_ISSUING_CARRIER(5,"Original 1 - (For Issuing Carrier)", true),
    COPY_5_FOR_DESTINATION_AIRPORT(6, "Copy 5 - (Airport of Destination)", false),
    COPY_10_FOR_CARRIER(7, "Copy 10 - (Extra Copy for Carrier)", false),
    COPY_6_FOR_THIRD_CARRIER(8, "Copy 6 - (Third Carrier)", false),
    COPY_11_FOR_CARRIER(9, "Copy 11 - (Extra Copy for Carrier)", false),
    COPY_7_FOR_CARRIER(10,"Copy 7 - (For Second Carrier)", false),
    COPY_12_FOR_CARRIER(11,"Copy 12 - (Extra Copy for Carrier)", false),
    COPY_8_FIRST_CARRIER(12, "Copy 8 - (First Carrier)", false);

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
