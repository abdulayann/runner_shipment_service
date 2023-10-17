package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class HblTermsConditionTemplateDto implements IRunnerRequest {
    @JsonProperty("TemplateCode")
    private String TemplateCode;
    @JsonProperty("TemplateFileName")
    private String TemplateFileName;
    @JsonProperty("IsFrontPrint")
    private Boolean IsFrontPrint;
    @JsonProperty("TypeOfHblPrint")
    private String TypeOfHblPrint;
    @JsonProperty("IsWaterMarkRequired")
    private Boolean IsWaterMarkRequired;
}
