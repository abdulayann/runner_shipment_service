package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

@SuppressWarnings("java:S115") //Suppressing Rename this constant name to match the regular expression
@Generated
public enum ChargeTypeCode {
    Client_Charge_Code(1),
    Fusion_Memo_Line(2),
    Vendor_Integration_Code(3),
    B2B_LCc(4),
    B2B_LCc_Name(5),
    B2B_Sub_Lcc(6),
    B2B_Sub_Lcc_Name(7),
    Pantos_Tax_Code(8),
    Revenue_Tax_Account_Code(9),
    Expense_Tax_Account_Code(10),
    IATA_Charge_Code(11),
    Due_To_Party(12),
    B2B_Tax_Code(13),
    Tax_Classification_Code(14),
    Fusion_Distribution_Set(15),
    Revenue_GL_Code(16),
    Revenue_VAT_GL_Code(17),
    Expense_GL_Code(18),
    Expense_VAT_GL_Code(19);

    int id;
    ChargeTypeCode(int id) {
        this.id = id;
    }

    @JsonValue
    public int getId() {
        return id;
    }

    @JsonCreator
    public static ChargeTypeCode fromId(int id) {
        for (ChargeTypeCode chargeTypeCode : ChargeTypeCode.values()) {
            if (chargeTypeCode.id == id) {
                return chargeTypeCode;
            }
        }
        throw new IllegalArgumentException("Invalid ChargeTypeCode id: " + id);
    }
}
