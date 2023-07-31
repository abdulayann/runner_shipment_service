package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.TypeOfHblPrint;
import io.swagger.annotations.ApiModel;
import lombok.*;

@Getter
@Setter
@ApiModel("Hbl Terms Condition Template Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class HblTermsConditionTemplateRequest extends CommonRequest implements IRunnerRequest {

    private Long id;
    private String templateCode;
    private String templateFileName;
    private Long shipmentSettingsId;
    private Boolean isFrontPrint;
    private TypeOfHblPrint typeOfHblPrint;
    private Boolean isWaterMarkRequired;
}
