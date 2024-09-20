package com.dpw.runner.shipment.services.entity.enums;

public enum TransportInstructionTemplateType {

    LC2C(1, "Consigner to Consignee"),
    EECS(2, "Export Empty to CFS"),
    EECR(3, "Export Empty to Consigner"),
    EEPS(4, "Export FCL/ULD pack at CFS"),
    EECSNE(5, "Export FCL/ULD pack at CFS no empty"),
    EEFCL(6, "Export FCL/ULD No empty"),
    EEFC(7, "Export FCL/ULD pack at Consigner"),
    EIPNE(8, "Export ICD to Port No Empty"),
    EFPCNE(9, "Export FCL Pack at Consigner No Empty"),
    EFTCC(10, "Export FTL CFS to Consignee"),
    CUSTOM(11, "Custom template Code"),
    EFTFT(12, "Export FTL Factory to Terminal"),
    IFTTF(13, "Import FTL Terminal to Factory"),
    IFTCF(14, "Import FTL CFS to Factory"),
    EFTFC(15, "Export FTL Factory to CFS"),
    IFTPF(16, "Import FTL  Port to Factory"),
    EFTFP(17, "Export FTL  Factory to Port"),
    EFTFI(18, "Export FTL Factory to ICD"),
    IFTIF(19, "Import FTL ICD to Factory"),
    EBARGE(20, "Export Barge"),
    IBARGE(21, "Import Barge"),
    ETFTP(22, "Export Factory to Port"),
    EITP(23, "Export ICD to Port"),
    IFPTF(24, "Import Port to Factory"),
    IFITP(2, "Import Port to ICD");
    private final int value;
    private final String description;

    TransportInstructionTemplateType(int value, String description) {
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
