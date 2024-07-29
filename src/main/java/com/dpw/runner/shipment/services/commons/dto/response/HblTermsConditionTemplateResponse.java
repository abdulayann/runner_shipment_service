package com.dpw.runner.shipment.services.commons.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.TypeOfHblPrint;
import lombok.Data;

@Data
public class HblTermsConditionTemplateResponse implements IRunnerResponse {

    private Long id;
    private String templateCode;
    private String templateFileName;
    private Long shipmentSettingsId;
    private Boolean isFrontPrint;
    private TypeOfHblPrint typeOfHblPrint;
    private Boolean isWaterMarkRequired;
}
