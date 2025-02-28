package com.dpw.runner.shipment.services.entity.enums;

import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

@Generated
public enum ChargeTypeCode {
    CHARGE_TYPE_CODE(1),
    FUSION_MEMO_LINE(2),
    VENDOR_INTEGRATION_CODE(3),
    B2B_LCC(4),
    B2B_LCC_NAME(5),
    B2B_SUB_LCC(6),
    B2B_SUB_LCC_NAME(7),
    PANTOS_TAX_CODE(8),
    REVENUE_TAX_ACCOUNT_CODE(9),
    EXPENSE_TAX_ACCOUNT_CODE(10),
    IATA_CHARGE_CODE(11),
    DUE_TO_PARTY(12),
    B2B_TAX_CODE(13),
    TAX_CLASSIFICATION_CODE(14),
    FUSION_DISTRIBUTION_SET(15),
    REVENUE_GL_CODE(16),
    REVENUE_VAT_GL_CODE(17),
    EXPENSE_GL_CODE(18),
    EXPENSE_VAT_GL_CODE(19);

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
