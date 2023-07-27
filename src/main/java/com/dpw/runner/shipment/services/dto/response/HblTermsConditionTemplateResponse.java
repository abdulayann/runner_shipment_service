package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.entity.enums.TypeOfHblPrint;
import lombok.Data;

@Data
public class HblTermsConditionTemplateResponse {

    private Long id;
    private String templateCode;
    private String templateFileName;
    private Long shipmentSettingsId;
    private Boolean isFrontPrint;
    private TypeOfHblPrint typeOfHblPrint;
    private Boolean isWaterMarkRequired;
}
